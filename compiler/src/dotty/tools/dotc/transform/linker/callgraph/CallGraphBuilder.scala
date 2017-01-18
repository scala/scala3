package dotty.tools.dotc.transform.linker.callgraph

import java.io.File

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.NameOps.SuperAccessorName
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols.{Symbol, _}
import dotty.tools.dotc.core.TypeErasure
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.ResolveSuper
import dotty.tools.dotc.transform.linker.summaries._
import dotty.tools.dotc.transform.linker.types._
import dotty.tools.dotc.transform.linker.CollectSummaries
import dotty.tools.dotc.util.WorkList

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}

object CallGraphBuilder {
  final val AnalyseOrig = 1
  final val AnalyseTypes = 2
  final val AnalyseArgs = 3
}

class CallGraphBuilder(collectedSummaries: Map[Symbol, MethodSummary], mode: Int, specLimit: Int)(implicit ctx: Context) {
  import CallGraphBuilder._
  import tpd._

  private val entryPoints = mutable.HashMap.empty[CallInfoWithContext, Int]
  private val reachableTypes = new WorkList[TypeWithContext]()
  private val reachableMethods = new WorkList[CallInfoWithContext]()
  private val outerMethods = mutable.Set[Symbol]()
  private val classOfs = new WorkList[Symbol]()
  private val casts = new WorkList[Cast]()

  private val typesByMemberNameCache = new java.util.IdentityHashMap[Name, Set[TypeWithContext]]()
  private val castsCache: mutable.HashMap[TypeWithContext, mutable.Set[Cast]] = mutable.HashMap.empty

  def pushEntryPoint(s: Symbol, entryPointId: Int): Unit = {
    val tpe = ref(s).tpe.asInstanceOf[TermRef]
    val targsSize = tpe.widen match {
      case t: PolyType => t.paramNames.size
      case _ => 0
    }
    val targs = (0 until targsSize).map(x => new ErazedType()).toList
    val args = ctx.definitions.ArrayOf(ctx.definitions.StringType) :: Nil
    val call = CallInfoWithContext(tpe, targs, args, OuterTargs.empty)(None, None)
    entryPoints(call) = entryPointId
    reachableMethods += call
    val t = ref(s.owner).tpe
    val self = new TypeWithContext(t, parentRefinements(t))
    addReachableType(self, null)
  }

  /** Builds the call graph based on the current reachable items, mainly entry points. */
  def build(): Unit = {
    ctx.log(s"\t Building call graph with ${reachableMethods.newItems.size} entry points")
    @tailrec def buildLoop(): Unit = {
      if (reachableMethods.hasNewItems || reachableTypes.hasNewItems || casts.hasNewItems) {
        reachableMethods.clearNewItems()
        reachableTypes.clearNewItems()
        casts.clearNewItems()
        classOfs.clearNewItems()

        processCallSites(reachableMethods.items, reachableTypes.items)

        val newReachableTypes = reachableTypes.newItems
        ctx.log(s"\t Found ${newReachableTypes.size} new instantiated types")
        val newClassOfs = classOfs.newItems
        ctx.log(s"\t Found ${newClassOfs.size} new classOfs ")
        newReachableTypes.foreach { x =>
          val clas = x.tp match {
            case t: ClosureType =>
              t.u.classSymbol.asClass
            case t: JavaAllocatedType =>
              t.underlying.widenDealias.classSymbol.asClass
            case _ => x.tp.classSymbol.asClass
          }
          if (!clas.is(JavaDefined) && clas.is(Module)) {
            val fields = clas.classInfo.decls.filter(x => !x.is(Method) && !x.isType)
            val parent = Some(CallInfoWithContext(x.tp.select(clas.primaryConstructor).asInstanceOf[TermRef], x.tp.baseArgInfos(clas), Nil, x.outerTargs)(None, None))
            reachableMethods ++= fields.map {
              fieldSym =>
                CallInfoWithContext(x.tp.select(fieldSym).asInstanceOf[TermRef], Nil, Nil, x.outerTargs)(parent, None)
            }
          }
        }

        val newReachableMethods = reachableMethods.newItems

        // val outFile =  new java.io.File(ctx.settings.d.value + s"/CallGraph-${reachableMethods.items.size}.html")
        // GraphVisualization.outputGraphVisToFile(this.result(), outFile)

        ctx.log(s"\t Found ${newReachableMethods.size} new call sites")
        buildLoop()
      }
    }

    buildLoop()
  }

  /** Packages the current call graph into a CallGraph */
  def result(): CallGraph = {
    val entryPoints = this.entryPoints.toMap
    val reachableMethods = this.reachableMethods.items
    val reachableTypes = this.reachableTypes.items
    val casts = this.casts.items
    val classOfs = this.classOfs.items
    val outerMethods = this.outerMethods.toSet
    CallGraph(entryPoints, reachableMethods, reachableTypes, casts, classOfs, outerMethods, mode, specLimit)
  }

  private def addReachableType(x: TypeWithContext, from: CallInfoWithContext): Unit = {
    if (!reachableTypes.contains(x)) {
      reachableTypes += x
      val namesInType = x.tp.memberNames(takeAllFilter).filter(typesByMemberNameCache.containsKey)
      for (name <- namesInType) {
        typesByMemberNameCache.put(name, typesByMemberNameCache.get(name) + x)
      }
    }
  }

  private def addReachableClosure(x: ClosureType, from: CallInfoWithContext): Unit = {
    val substitution = new SubstituteByParentMap(from.outerTargs)
    val tp = substitution(x)

    val ctp = tp match {
      case t: ClosureType => new TypeWithContext(t, t.outerTargs ++ parentRefinements(t))
      case _ => new TypeWithContext(tp, parentRefinements(tp))
    }

    addReachableType(ctp, from)
  }

  private def getTypesByMemberName(x: Name): Set[TypeWithContext] = {
    val ret1 = typesByMemberNameCache.get(x)
    if (ret1 eq null) {
      // not yet computed
      val upd = reachableTypes.items.filter(tp => tp.tp.member(x).exists)
      typesByMemberNameCache.put(x, upd)
      upd
    } else ret1
  }

  private def addCast(from: Type, to: Type) = {
    if (!(from <:< to) && to.classSymbols.forall(!_.derivesFrom(defn.NothingClass))) {
      val newCast = Cast(from, to)
      for (tp <- reachableTypes.items) {
        if (from.classSymbols.forall(x => tp.tp.classSymbols.exists(y => y.derivesFrom(x))) && to.classSymbols.forall(x => tp.tp.classSymbols.exists(y => y.derivesFrom(x)))) {
          casts += newCast
          castsCache.getOrElseUpdate(tp, mutable.Set.empty) += newCast
        }
      }
    }
  }

  private def registerParentModules(tp: Type, from: CallInfoWithContext): Unit = {
    @tailrec def register(tp1: Type): Unit = {
      if ((tp1 ne NoType) && (tp1 ne NoPrefix)) {
        if (tp1.widen ne tp1) registerParentModules(tp1.widen, from)
        if (tp1.dealias ne tp1) registerParentModules(tp1.dealias, from)
        if (tp1.termSymbol.is(Module)) {
          addReachableType(new TypeWithContext(tp1.widenDealias, parentRefinements(tp1.widenDealias)), from)
        } else if (tp1.typeSymbol.is(Module, Package)) {
          val t = ref(tp1.typeSymbol).tpe
          addReachableType(new TypeWithContext(t, parentRefinements(t)), from)
        }
        register(tp1.normalizedPrefix)
      }
    }
    register(tp)
  }

  private def parentRefinements(tp: Type)(implicit ctx: Context): OuterTargs = {
    new TypeAccumulator[OuterTargs]() {
      def apply(x: OuterTargs, tp: Type): OuterTargs = tp match {
        case t: RefinedType =>
          val member = t.parent.member(t.refinedName).symbol
          val parent = member.owner
          // val tparams = parent.info.typeParams
          // val id = tparams.indexOf(member)
          // assert(id >= 0) // TODO: IS this code needed at all?

          val nList = x.add(parent, t.refinedName, t.refinedInfo)
          apply(nList, t.parent)
        case t: ClosureType =>
          apply(x, t.u)
        case _ =>
          foldOver(x, tp)
      }
    }.apply(OuterTargs.empty, tp)
  }

  private def instantiateCallSite(caller: CallInfoWithContext, rec: Type, callee: CallInfo, types: Traversable[TypeWithContext]): Traversable[CallInfoWithContext] = {

    lazy val someCaller = Some(caller)
    lazy val someCallee = Some(callee)

    val receiver = callee.call.normalizedPrefix
    registerParentModules(receiver, caller)

    val calleeSymbol = callee.call.termSymbol.asTerm
    val callerSymbol = caller.call.termSymbol

    val tpamsOuter = caller.call.widen match {
      case t: PolyType =>
        OuterTargs.empty.addAll(callerSymbol, t.paramNames, caller.targs)
      case _ =>
        OuterTargs.empty
    }

    lazy val outerParent = if (callerSymbol.owner ne caller.call.normalizedPrefix.classSymbol) {
      val current = caller.call.normalizedPrefix
      val superTpe = callerSymbol.owner.info
      val outers = current.typeMembers.foldLeft(OuterTargs.empty) { (outerTargs: OuterTargs, x) =>
        val old = superTpe.member(x.symbol.name)
        if (old.exists) outerTargs.add(callerSymbol.owner, x.symbol.name, x.info) else outerTargs
      }
      outers
    } else OuterTargs.empty

    lazy val outerPropagetedTargs = caller.outerTargs ++ tpamsOuter ++ outerParent
    lazy val substitution = new SubstituteByParentMap(outerPropagetedTargs)

    def propagateArgs(tp: Type): Type = {
      tp match {
        case x: TermRef if mode >= AnalyseArgs && x.symbol.is(Param) && x.symbol.owner == caller.call.termSymbol =>
          val id = caller.call.termSymbol.info.paramNamess.flatten.indexWhere(_ == x.symbol.name)
          caller.argumentsPassed(id)
        case t => t
      }
    }

    def propagateTargs(tp0: Type, isConstructor: Boolean = false): Type = {
      val tp = propagateArgs(tp0)
      if (mode >= AnalyseTypes && (caller.targs.nonEmpty || caller.outerTargs.nonEmpty || (callerSymbol.owner ne caller.call.normalizedPrefix.classSymbol))) {
        /* && tp.widenDealias.existsPart{x => val t = x.typeSymbol; t.exists && (t.owner == callerSymbol || caller.outerTargs.contains(t.owner))}*/

        val refinedClassType = if (isConstructor) {
          val refinedConstructedType = tp.typeMembers.foldLeft(tp){(acc, memberType) =>
            val refinedInfo = substitution.apply(memberType.info)
            if (refinedInfo ne memberType.info) RefinedType(acc, memberType.symbol.name, refinedInfo)
            else acc
          }
          refinedConstructedType
        } else if (mode >= AnalyseArgs && (tp.isInstanceOf[PreciseType] || tp.isInstanceOf[ClosureType])) tp
        else tp.widenDealias
        val r = substitution.apply(refinedClassType)
        // for simplification only
        if (r =:= tp) tp
        else r
      } else tp
    }

    val outerTargs: OuterTargs =
      if (mode < AnalyseTypes) OuterTargs.empty
      else if (calleeSymbol.isProperlyContainedIn(callerSymbol)) {
        parentRefinements(propagateTargs(receiver)) ++ caller.outerTargs ++ tpamsOuter
      } else {
        parentRefinements(propagateTargs(receiver)) ++ new OuterTargs(caller.outerTargs.mp.filter(x => calleeSymbol.isProperlyContainedIn(x._1)))
        // todo: Is AsSeenFrom ever needed for outerTags?
      }

    // if typearg of callee is a typeparam of caller, propagate typearg from caller to callee
    lazy val targs = callee.targs map {
      case t: TypeVar if mode >= AnalyseTypes && t.stripTypeVar.typeSymbol.maybeOwner == caller.call.termSymbol =>
        assert(caller.call.termSymbol.exists)
        val abstractSym = t.stripTypeVar.typeSymbol
        val id = caller.call.termSymbol.info.asInstanceOf[PolyType].paramNames.indexOf(abstractSym.name)
        propagateTargs(caller.targs(id).stripTypeVar)
      case t if mode >= AnalyseTypes=> propagateTargs(t.stripTypeVar)
      case t => t.stripTypeVar
    }
    // if arg of callee is a param of caller, propagate arg fro caller to callee
    val args = callee.argumentsPassed.map {
      case x if mode < AnalyseArgs =>
        val s = x.widenDealias.finalResultType.classSymbol.orElse(TypeErasure.erasure(x.widenDealias.finalResultType).classSymbol)
        assert(s.exists)
        ref(s).tpe
      case x: PreciseType =>
        x
      case x: ClosureType =>
        val utpe =  propagateTargs(x.underlying, isConstructor = true)
        val outer = parentRefinements(utpe) ++ outerTargs
        val closureT = new ClosureType(x.meth, utpe, x.implementedMethod, outer)
        addReachableType(new TypeWithContext(closureT, outer), caller)
        closureT
      case x: TermRef if x.symbol.is(Param) && x.symbol.owner == caller.call.termSymbol =>  // todo: we could also handle outer arguments
        val id = caller.call.termSymbol.info.paramNamess.flatten.indexWhere(_ == x.symbol.name)
        caller.argumentsPassed(id)
      case x => propagateTargs(x)
    }

    def filterTypes(tp1: Type, tp2: Type): Boolean = {
      if (mode >= AnalyseTypes) tp1 <:< tp2
      else {
        val tp1w = tp1.widenDealias
        val tp2w = tp2.widenDealias
        val tp2c = tp2w.classSymbol.orElse(TypeErasure.erasure(tp2).classSymbol)
        tp1w.derivesFrom(tp2c)
      }
    }

    def appliedToTargs(tp: Type): Type =
      tp.widen.appliedTo(targs).widen

    def preciseSelectCall(tp: Type, callSym: Symbol): TermRef = {
      val selectByName = tp.select(callSym.name)
      val call = selectByName.asInstanceOf[TermRef].denot.suchThat(x =>
        x == callSym || x.overriddenSymbol(callSym.owner.asClass) == callSym).symbol
      assert(call.exists)
      tp.select(call).asInstanceOf[TermRef]
    }

    def dispatchCalls(receiverType: Type): Traversable[CallInfoWithContext] = {
      receiverType match {
        case t: PreciseType =>
          CallInfoWithContext(preciseSelectCall(t.underlying, calleeSymbol), targs, args, outerTargs)(someCaller, someCallee) :: Nil
        case t: ClosureType if calleeSymbol.name eq t.implementedMethod.name =>
          val methodSym = t.meth.meth.symbol.asTerm
          CallInfoWithContext(TermRef.withFixedSym(t.underlying, methodSym.name,  methodSym), targs, t.meth.env.map(_.tpe) ++ args, outerTargs ++ t.outerTargs)(someCaller, someCallee) :: Nil
        case AndType(tp1, tp2) =>
          dispatchCalls(tp1).toSet.intersect(dispatchCalls(tp2).toSet)
        case _ =>
          // without casts
          val receiverTypeWiden = receiverType.widenDealias match {
            case t: TypeRefWithFixedSym if t.classSymbol.owner.is(Method) =>
              new TypeRefWithFixedSym(t.classSymbol.owner.owner.typeRef, t.name, t.fixedSym)
            case t => t
          }

          val direct = {
            for {
              tp <- getTypesByMemberName(calleeSymbol.name)
              if filterTypes(tp.tp, receiverTypeWiden)
              alt <- tp.tp.member(calleeSymbol.name).altsWith(p => p.asSeenFrom(tp.tp).matches(calleeSymbol.asSeenFrom(tp.tp)))
              if alt.exists
            } yield {
              CallInfoWithContext(tp.tp.select(alt.symbol).asInstanceOf[TermRef], targs, args, outerTargs ++ tp.outerTargs)(someCaller, someCallee)
            }
          }

          val casted = if (mode < AnalyseTypes) {
            Nil
          } else {
            def filterCast(cast: Cast) = {
              val receiverBases = receiverType.classSymbols
              val targetBases = cast.to.classSymbols
              receiverBases.forall(c => targetBases.exists(_.derivesFrom(c)))
            }
            for {
              tp <- getTypesByMemberName(calleeSymbol.name)
              cast <- castsCache.getOrElse(tp, Iterator.empty)
              if /*filterTypes(tp.tp, cast.from) &&*/ filterTypes(cast.to, receiverType) && filterCast(cast)
              alt <- tp.tp.member(calleeSymbol.name).altsWith(p => p.matches(calleeSymbol.asSeenFrom(tp.tp)))
              if alt.exists
            } yield {
              // this additionaly introduces a cast of result type and argument types
              val preciseCall = preciseSelectCall(tp.tp, alt.symbol)
              val uncastedSig = appliedToTargs(preciseCall)
              val castedSig = appliedToTargs(preciseSelectCall(receiverType, calleeSymbol))

              (uncastedSig.paramTypess.flatten zip castedSig.paramTypess.flatten) foreach (x => addCast(x._2, x._1))
              addCast(uncastedSig.finalResultType, castedSig.finalResultType)

              CallInfoWithContext(preciseCall.asInstanceOf[TermRef], targs, args, outerTargs ++ tp.outerTargs)(someCaller, someCallee)
            }
          }

          direct ++ casted
      }
    }

    receiver match {
      case _ if calleeSymbol == defn.Predef_classOf =>
        classOfs += callee.targs.head.classSymbol
        Nil
      case _ if calleeSymbol == ctx.definitions.throwMethod =>
        Nil
      case _ if calleeSymbol == ctx.definitions.Any_asInstanceOf =>
        val from = propagateTargs(receiver)
        val to = propagateTargs(targs.head)
        addCast(from, to)
        Nil
      // TODO: handle == and !=
      case _ if defn.ObjectMethods.contains(calleeSymbol) || defn.AnyMethods.contains(calleeSymbol) =>
        // TODO: only for paper
        Nil
      case NoPrefix =>  // inner method
        assert(calleeSymbol.is(ParamAccessor) || calleeSymbol.owner.is(Method) || calleeSymbol.owner.isLocalDummy)
        CallInfoWithContext(TermRef.withFixedSym(caller.call.normalizedPrefix, calleeSymbol.name, calleeSymbol), targs, args, outerTargs)(someCaller, someCallee) :: Nil

      case t if calleeSymbol.isConstructor =>

        val constructedType = appliedToTargs(callee.call).resultType
        val fixNoPrefix = if (constructedType.normalizedPrefix eq NoPrefix) {
          @tailrec def getPrefix(currentPrefix: Type): Type = {
            if (currentPrefix.classSymbol.exists) currentPrefix
            else if (currentPrefix.termSymbol.is(Module)) getPrefix(currentPrefix.widenDealias)
            else {
              getPrefix(currentPrefix.normalizedPrefix match {
                case t: ThisType => t.tref
                case t => t
              })
            }
          }
          constructedType match {
            case constructedType @ TypeRef(prefix, name)  =>
              constructedType.underlying match {
                case ci: ClassInfo =>
                  val currentPrefix = getPrefix(caller.call.normalizedPrefix)
                  val nci = ci.derivedClassInfo(prefix = currentPrefix) // todo: do this only for inner anonym classes
                  TypeRef.withFixedSymAndInfo(currentPrefix, name, constructedType.symbol.asType, nci)
              }
          }
        } else constructedType

        val tpe =  propagateTargs(fixNoPrefix, isConstructor = true)
        addReachableType(new TypeWithContext(tpe, parentRefinements(tpe) ++ outerTargs), caller)

        val call = {
          if (callerSymbol.isConstructor && callerSymbol.owner == calleeSymbol.owner)
            new TermRefWithFixedSym(calleeSymbol.owner.typeRef, calleeSymbol.name, calleeSymbol)
          else
            propagateTargs(receiver).select(calleeSymbol).asInstanceOf[TermRef]
        }

        CallInfoWithContext(call, targs, args, outerTargs)(someCaller, someCallee) :: Nil

      // super call in a class (know target precisely)
      case st: SuperType =>
        val thisTpe = st.thistpe
        val targetClass = st.supertpe.baseClasses.find(clz =>
          clz.info.decl(calleeSymbol.name).altsWith(p => p.signature == calleeSymbol.signature).nonEmpty
        )
        val targetMethod = targetClass.get.info.member(calleeSymbol.name).altsWith(p => p.signature == calleeSymbol.signature).head.symbol.asTerm
        val thisTpePropagated = propagateTargs(thisTpe)


        CallInfoWithContext(TermRef.withFixedSym(thisTpePropagated, targetMethod.name, targetMethod), targs, args, outerTargs)(someCaller, someCallee) :: Nil

      // super call in a trait
      case t if calleeSymbol.is(SuperAccessor) =>

        // Taken from ResolveSuper.rebindSuper
        val unexpandedAccName =
          if (calleeSymbol.is(ExpandedName))  // Cannot use unexpandedName because of #765. t2183.scala would fail if we did.
            calleeSymbol.name
              .drop(calleeSymbol.name.indexOfSlice(nme.EXPAND_SEPARATOR ++ nme.SUPER_PREFIX))
              .drop(nme.EXPAND_SEPARATOR.length)
          else calleeSymbol.name

        val SuperAccessorName(memberName) = unexpandedAccName: Name

        val prev = t.widenDealias.classSymbol
        getTypesByMemberName(memberName).flatMap {
          x =>
            val s = x.tp.baseClasses.dropWhile(_ != prev)
            if (s.nonEmpty) {
              val parentMethod = ResolveSuper.rebindSuper(x.tp.widenDealias.classSymbol, calleeSymbol).asTerm
              // todo: outerTargs are here defined in terms of location of the subclass. Is this correct?
              CallInfoWithContext(TermRef.withFixedSym(t, parentMethod.name, parentMethod), targs, args, outerTargs)(someCaller, someCallee) :: Nil

            } else Nil
        }

      case thisType: ThisType if !calleeSymbol.owner.flags.is(PackageCreationFlags) =>
        val dropUntil = thisType.tref.classSymbol
        @tailrec def getPreciseThis(currentThis: Type, currentOwner: Symbol): Type = {
          if ((currentOwner eq dropUntil) || currentOwner.owner.is(Package) || (currentThis eq NoType)) currentThis
          else if (currentOwner.is(Method)) getPreciseThis(currentThis, currentOwner.owner.enclosingClass)
          else getPreciseThis(currentThis.normalizedPrefix, currentOwner.owner.enclosingClass)
        }

        val currentThis = getPreciseThis(caller.call.normalizedPrefix, caller.call.termSymbol.owner)

        if (!currentThis.derivesFrom(thisType.cls))
          dispatchCalls(propagateTargs(receiver.widenDealias))
        else if (calleeSymbol.is(Private))
          CallInfoWithContext(TermRef.withFixedSym(currentThis, calleeSymbol.name, calleeSymbol), targs, args, outerTargs)(someCaller, someCallee) :: Nil
        else
          dispatchCalls(propagateTargs(AndType.apply(currentThis, thisType.tref)))


      case _: PreciseType =>
        dispatchCalls(propagateTargs(receiver))
      case _: ClosureType =>
        dispatchCalls(propagateTargs(receiver))
      case x: TermRef if x.symbol.is(Param) && x.symbol.owner == caller.call.termSymbol =>
        dispatchCalls(propagateTargs(receiver))
      case _ =>
        dispatchCalls(propagateTargs(receiver.widenDealias))
    }
  }

  private def processCallSites(callSites: immutable.Set[CallInfoWithContext], instantiatedTypes: immutable.Set[TypeWithContext]): Unit = {
    for (method <- callSites) {
      // Find new call sites

      collectedSummaries.get(method.callSymbol) match {
        case Some(summary) =>
          summary.accessedModules.foreach(x => addReachableType(new TypeWithContext(x.info, parentRefinements(x.info)), method))
          summary.definedClosures.foreach(x => addReachableClosure(x, method))
          summary.methodsCalled.foreach {
            case (receiver, theseCallSites) => theseCallSites.foreach { callSite =>
              val nw = instantiateCallSite(method, receiver, callSite, instantiatedTypes)
              reachableMethods ++= nw
              method.addOutEdges(callSite, nw)
            }
          }

        case None =>
          outerMethods += method.callSymbol
          if (!method.call.termSymbol.is(Module | Package) && !method.parent.exists(_.isOnJavaAllocatedType)) {
            // Add all possible calls from java to object passed as parameters.
            processCallsFromJava(method, instantiatedTypes)
          }
      }
    }
  }

  private def processCallsFromJava(method: CallInfoWithContext, instantiatedTypes: immutable.Set[TypeWithContext]): Unit = {

    val tParamNames = method.call.widenDealias.typeParams.map(_.paramName)
    val newOuterTargs = method.outerTargs.addAll(method.callSymbol, tParamNames, method.targs)

    def substituteOuterTargs = new SubstituteByParentMap(newOuterTargs)

    // Add return type to reachable types
    val methodTpe = method.call.widenDealias

    val returnType = methodTpe match {
      case t: PolyType => t.instantiate(method.targs).finalResultType
      case _ => methodTpe.finalResultType
    }

    val javaAllocatedType = returnType match {
      case returnType: JavaAllocatedType => returnType
      case returnType: HKApply => new JavaAllocatedType(substituteOuterTargs(returnType.tycon.appliedTo(method.targs)))
      case returnType => new JavaAllocatedType(substituteOuterTargs(returnType))
    }
    addReachableType(new TypeWithContext(javaAllocatedType, OuterTargs.empty), method)

    def allPotentialCallsFor(argType: Type): Set[CallInfo] = {
      if (defn.isPrimitiveClass(argType.classSymbol)) {
        Set.empty
      } else {
        def potentialCall(decl: Symbol): Option[CallInfo] = {
          def paramTypes = decl.info.paramTypess.flatten
          val call = new TermRefWithFixedSym(argType, decl.name.asTermName, decl.asTerm)
          val targs = call.widenDealias match {
            case call: PolyType =>
              def erasedBounds(tp: TypeBounds): Type = tp.hi match {
                case RefinedType(parent, refinedName, refinedInfo: TypeBounds) =>
                  RefinedType(parent, refinedName, erasedBounds(refinedInfo))
                case t => t
              }
              call.paramBounds.map(erasedBounds)

            case _ => Nil
          }

          def isDefinedInJavaClass(sym: Symbol) =
            sym.owner == defn.AnyClass || sym.owner.is(JavaDefined)

          val definedInJavaClass: Boolean =
            isDefinedInJavaClass(decl) || decl.allOverriddenSymbols.exists(isDefinedInJavaClass)

          argType match {
            case argType: PreciseType =>
              if (!definedInJavaClass) None
              else Some(CallInfo(call, targs, paramTypes))

            case _ =>
              val argTypeWiden = argType.widenDealias
              lazy val sym = argTypeWiden.classSymbol.requiredMethod(decl.name, paramTypes)
              if (paramTypes.exists(_.typeSymbol.isTypeParam)) {
                // println(s"Ignoring `${decl.name}` in java call graph construction because type parameters are not suported yet")
                None
              } else if (!argTypeWiden.member(decl.name).exists || !definedInJavaClass || (isDefinedInJavaClass(decl) && sym.isEffectivelyFinal)) None
              else Some(CallInfo(TermRef(argType, sym), targs, paramTypes))

          }
        }

        def allDecls(argType: Type) =
          (argType.decls ++ argType.parents.flatMap(_.decls)).toSet

        for {
          decl <- allDecls(argType)
          if decl.isTerm && !decl.isConstructor && decl.name != nme.COMPANION_MODULE_METHOD
          if decl.name != nme.isInstanceOf_ && decl.name != nme.asInstanceOf_ && decl.name != nme.synchronized_
          callInfo <- potentialCall(decl)
        } yield callInfo
      }
    }

    for {
      rec <- (method.call.normalizedPrefix :: method.argumentsPassed).distinct
      potentialCall <- allPotentialCallsFor(rec)
      if method.getOutEdges(potentialCall).isEmpty
    } {
      val nw = instantiateCallSite(method, rec, potentialCall, instantiatedTypes)
      reachableMethods ++= nw
      method.addOutEdges(potentialCall, nw)
    }
  }

}
