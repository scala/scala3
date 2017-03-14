package dotty.tools.dotc.transform.linker.callgraph

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.NameOps.SuperAccessorName
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols.{Symbol, _}
import dotty.tools.dotc.core.TypeErasure
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.ResolveSuper
import dotty.tools.dotc.transform.linker.summaries._
import dotty.tools.dotc.transform.linker.types._
import dotty.tools.dotc.util.WorkList

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}

object CallGraphBuilder {
  final val AnalyseOrig = 1
  final val AnalyseTypes = 2
  final val AnalyseArgs = 3
}

class CallGraphBuilder(collectedSummaries: Map[Symbol, MethodSummary], mode: Int, specLimit: Int,
    withJavaCallGraph: Boolean)(implicit ctx: Context) {
  import CallGraphBuilder._
  import tpd._

  private var iteration = 0

  private val entryPoints = mutable.HashMap.empty[CallInfoWithContext, Int]
  private val reachableTypes = new WorkList[TypeWithContext]()
  private val reachableMethods = new WorkList[CallInfoWithContext]()
  private val outerMethods = mutable.Set[Symbol]()
  private val classOfs = new WorkList[Symbol]()
  private val casts = new WorkList[Cast]()

  private val typesByMemberNameCache = new java.util.IdentityHashMap[Name, List[TypeWithContext]]()
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
    addReachableType(self)
  }

  /** Builds the call graph based on the current reachable items, mainly entry points. */
  def build(): Unit = {
    ctx.log(s"Building call graph with ${reachableMethods.newItems.size} entry points")
    @tailrec def buildLoop(): Unit = {
      if (reachableMethods.hasNewItems || reachableTypes.hasNewItems || casts.hasNewItems) {
        reachableMethods.clearNewItems()
        reachableTypes.clearNewItems()
        casts.clearNewItems()
        classOfs.clearNewItems()

        processCallSites()

        val newReachableTypes = reachableTypes.newItems
        iteration += 1
        ctx.log(s"Graph building iteration $iteration")
        ctx.log(s"Found ${newReachableTypes.size} new instantiated types")
        val newClassOfs = classOfs.newItems
        ctx.log(s"Found ${newClassOfs.size} new classOfs")
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

        ctx.log(s"Found ${newReachableMethods.size} new call sites")
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
    new CallGraph(entryPoints, reachableMethods, reachableTypes, casts, classOfs, outerMethods, mode, specLimit)
  }

  private def addReachableType(x: TypeWithContext): Unit = {
    if (!reachableTypes.contains(x)) {
      reachableTypes += x
      val namesInType = x.tp.memberNames(takeAllFilter).filter(typesByMemberNameCache.containsKey)
      for (name <- namesInType) {
        typesByMemberNameCache.put(name, x :: typesByMemberNameCache.get(name))
      }
    }
  }

  private def addReachableClosure(x: ClosureType, from: CallInfoWithContext): Unit = {
    val substitution = new SubstituteByParentMap(from.outerTargs)
    val tp = substitution(x)

    val ctp = tp match {
      case t: ClosureType => new TypeWithContext(t, parentRefinements(t))
      case _ => new TypeWithContext(tp, parentRefinements(tp))
    }

    addReachableType(ctp)
  }

  private def getTypesByMemberName(x: Name): List[TypeWithContext] = {
    val ret1 = typesByMemberNameCache.get(x)
    if (ret1 eq null) {
      // not yet computed
      val upd = reachableTypes.itemsIterator.filter(tp => tp.tp.member(x).exists).toList
      typesByMemberNameCache.put(x, upd)
      upd
    } else ret1
  }

  private def addCast(from: Type, to: Type) = {
    if (!(from <:< to) && to.classSymbols.forall(!_.derivesFrom(defn.NothingClass))) {
      val newCast = new Cast(from, to)
      for (tp <- reachableTypes.items) {
        if (from.classSymbols.forall(x => tp.tp.classSymbols.exists(y => y.derivesFrom(x))) && to.classSymbols.forall(x => tp.tp.classSymbols.exists(y => y.derivesFrom(x)))) {
          casts += newCast
          castsCache.getOrElseUpdate(tp, mutable.Set.empty) += newCast
        }
      }
    }
  }

  private val registeredTypes = mutable.Set.empty[Type]
  private def registerParentModules(tp: Type): Unit = {
    if ((tp ne NoType) && (tp ne NoPrefix) && !registeredTypes.contains(tp)) {
      if (tp.widen ne tp) registerParentModules(tp.widen)
      if (tp.dealias ne tp) registerParentModules(tp.dealias)
      if (tp.termSymbol.is(Module)) {
        addReachableType(new TypeWithContext(tp.widenDealias, parentRefinements(tp.widenDealias)))
      } else if (tp.typeSymbol.is(Module, butNot = Package)) {
        val t = ref(tp.typeSymbol).tpe
        addReachableType(new TypeWithContext(t, parentRefinements(t)))
      }
      registeredTypes += tp
      registerParentModules(tp.normalizedPrefix): @tailrec
    }
  }

  private def parentRefinements(tp: Type)(implicit ctx: Context): OuterTargs = {
    new TypeAccumulator[OuterTargs]() {
      def apply(x: OuterTargs, tp: Type): OuterTargs = tp match {
        case t: RefinedType =>
          val member = t.parent.member(t.refinedName).symbol
          val parent = member.owner
          val nList = x.add(parent, t.refinedName, t.refinedInfo)
          apply(nList, t.parent)
        case t: ClosureType =>
          apply(x, t.u)
        case _ =>
          foldOver(x, tp)
      }
    }.apply(OuterTargs.empty, tp)
  }

  private def instantiateCallSite(caller: CallInfoWithContext, rec: Type, callee: CallInfo): Traversable[CallInfoWithContext] = {

    lazy val someCaller = Some(caller)
    lazy val someCallee = Some(callee)

    val receiver = callee.call.normalizedPrefix
    registerParentModules(receiver)

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
      @tailrec def collectTypeMembers(classes: List[ClassSymbol], acc: Map[Name, Symbol]): Iterable[Symbol] = classes match {
        case x :: xs =>
          val typeDecls = x.unforcedDecls.filter(_.isType)
          val acc2 = typeDecls.foldLeft(acc)((acc1, sym) => if (acc1.contains(sym.name)) acc1 else acc1.updated(sym.name, sym))
          collectTypeMembers(xs, acc2)
        case Nil => acc.values
      }
      val typeMembers = collectTypeMembers(current.baseClasses, Map.empty)
      val outers = typeMembers.foldLeft(OuterTargs.empty) { (outerTargs: OuterTargs, x) =>
        val old = superTpe.member(x.symbol.name)
        if (old.exists) outerTargs.add(callerSymbol.owner, x.symbol.name, current.select(x).widen) else outerTargs
      }
      outers
    } else OuterTargs.empty

    lazy val outerPropagatedTargs = caller.outerTargs ++ tpamsOuter ++ outerParent
    lazy val substitution = new SubstituteByParentMap(outerPropagatedTargs)

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
        val closureT = new ClosureType(x.meth, utpe, x.implementedMethod)
        addReachableType(new TypeWithContext(closureT, outer))
        closureT
      case x: TermRef if x.symbol.is(Param) && x.symbol.owner == caller.call.termSymbol =>
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

    def preciseSelectCall(tp: Type, sym: Symbol): TermRef = {
      def selectCall(tp1: Type) = {
        if (sym.is(Private) || tp.typeSymbol == sym.owner) sym
        else {
          val selectByName = tp1.nonPrivateMember(sym.name)
          selectByName.suchThat(x => x == sym || x.overriddenSymbol(sym.owner.asClass) == sym).symbol
        }
      }
      val call = selectCall(tp).orElse(if (tp.givenSelfType.exists) selectCall(tp.givenSelfType) else NoSymbol)
      assert(call.exists, (tp, sym))
      tp.select(call).asInstanceOf[TermRef]
    }

    lazy val typesByMemberName = getTypesByMemberName(calleeSymbol.name)

    def dispatchCalls(receiverType: Type): Traversable[CallInfoWithContext] = {
      receiverType match {
        case t: PreciseType =>
          CallInfoWithContext(preciseSelectCall(t.underlying, calleeSymbol), targs, args, outerTargs)(someCaller, someCallee) :: Nil
        case t: ClosureType if calleeSymbol.name eq t.implementedMethod.name =>
          val methodSym = t.meth.meth.symbol.asTerm
          CallInfoWithContext(TermRef.withFixedSym(t.underlying, methodSym.name,  methodSym), targs, t.meth.env.map(_.tpe) ++ args, outerTargs)(someCaller, someCallee) :: Nil
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
              tp <- typesByMemberName
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
              tp <- typesByMemberName
              cast <- castsCache.getOrElse(tp, Iterator.empty)
              if filterTypes(tp.tp, cast.from) && filterTypes(cast.to, receiverType) && filterCast(cast)
              alt <- tp.tp.member(calleeSymbol.name).altsWith(p => p.matches(calleeSymbol.asSeenFrom(tp.tp)))
              if alt.exists
            } yield {
              // this additionally introduces a cast of result type and argument types
              val uncastedSig = preciseSelectCall(tp.tp, alt.symbol).widen.appliedTo(targs).widen
              val castedSig = preciseSelectCall(receiverType, calleeSymbol).widen.appliedTo(targs).widen
              (uncastedSig.paramTypess.flatten zip castedSig.paramTypess.flatten) foreach (x => addCast(x._2, x._1))
              addCast(uncastedSig.finalResultType, castedSig.finalResultType)

              CallInfoWithContext(tp.tp.select(alt.symbol).asInstanceOf[TermRef], targs, args, outerTargs ++ tp.outerTargs)(someCaller, someCallee)
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
      case _ if defn.ObjectMethods.contains(calleeSymbol) || defn.AnyMethods.contains(calleeSymbol) => Nil
      case NoPrefix =>  // inner method
        assert(calleeSymbol.is(ParamAccessor) || calleeSymbol.owner.is(Method) || calleeSymbol.owner.isLocalDummy)
        CallInfoWithContext(TermRef.withFixedSym(caller.call.normalizedPrefix, calleeSymbol.name, calleeSymbol), targs, args, outerTargs)(someCaller, someCallee) :: Nil

      case t if calleeSymbol.isConstructor =>

        val constructedType = appliedToTargs(callee.call).finalResultType
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
        addReachableType(new TypeWithContext(tpe, parentRefinements(tpe) ++ outerTargs))

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

  private def processCallSites(): Unit = {
    for (method <- reachableMethods.items) {
      // Find new call sites

      collectedSummaries.get(method.callSymbol) match {
        case Some(summary) =>
          summary.accessedModules.foreach(x => addReachableType(new TypeWithContext(x.info, parentRefinements(x.info))))
          summary.definedClosures.foreach(x => addReachableClosure(x, method))
          summary.methodsCalled.foreach {
            case (receiver, theseCallSites) => theseCallSites.foreach { callSite =>
              val instantiatedCalls = instantiateCallSite(method, receiver, callSite)
              reachableMethods ++= instantiatedCalls
              method.addOutEdges(callSite, instantiatedCalls)
            }
          }

        case None =>
          if (withJavaCallGraph && !method.call.termSymbol.is(Module | Package) && !method.parent.exists(_.isOnJavaAllocatedType)) {
            // Add all possible calls from java to object passed as parameters.
            outerMethods += method.callSymbol
            processCallsFromJava(method)
          }
      }
    }
  }

  private def processCallsFromJava(method: CallInfoWithContext): Unit = {

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
    addReachableType(new TypeWithContext(javaAllocatedType, OuterTargs.empty))

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
                  RefinedType(parent, refinedName, TypeAlias(erasedBounds(refinedInfo)))
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
      val instantiatedCalls = instantiateCallSite(method, rec, potentialCall)
      val instantiatedCallsToDefinedMethods = instantiatedCalls.filter(x => collectedSummaries.contains(x.callSymbol))
      reachableMethods ++= instantiatedCallsToDefinedMethods
      method.addOutEdges(potentialCall, instantiatedCallsToDefinedMethods)
    }
  }

}
