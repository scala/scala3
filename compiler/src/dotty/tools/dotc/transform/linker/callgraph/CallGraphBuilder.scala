package dotty.tools.dotc.transform.linker.callgraph

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
import dotty.tools.dotc.transform.linker.CollectSummaries.SubstituteByParentMap
import dotty.tools.dotc.transform.linker.summaries._
import dotty.tools.dotc.transform.linker.types._
import dotty.tools.dotc.transform.linker.{CollectSummaries, WorkList}

import scala.collection.{immutable, mutable}

object CallGraphBuilder {
  final val AnalyseOrig = 1
  final val AnalyseTypes = 2
  final val AnalyseArgs = 3
}

class CallGraphBuilder(mode: Int)(implicit ctx: Context) {
  import CallGraphBuilder._
  import tpd._

  private val collectedSummaries = ctx.summariesPhase.asInstanceOf[CollectSummaries].methodSummaries.map(x => (x.methodDef, x)).toMap

  private val entryPoints = mutable.HashMap.empty[CallWithContext, Int]
  private val reachableTypes = new WorkList[TypeWithContext]()
  private val reachableMethods = new WorkList[CallWithContext]()
  private val outerMethods = mutable.Set[Symbol]()
  private val classOfs = new WorkList[Symbol]()
  private val casts = new WorkList[Cast]()

  private val typesByMemberNameCache = new java.util.IdentityHashMap[Name, Set[TypeWithContext]]()

  def pushEntryPoint(s: Symbol, entryPointId: Int): Unit = {
    val tpe = ref(s).tpe
    val targsSize = tpe.widen match {
      case t: PolyType => t.paramNames.size
      case _ => 0
    }
    val targs = (0 until targsSize).map(x => new ErazedType()).toList
    val args = ctx.definitions.ArrayOf(ctx.definitions.StringType) :: Nil
    val call = new CallWithContext(tpe, targs, args, OuterTargs.empty, null, null)
    entryPoints(call) = entryPointId
    reachableMethods += call
    val t = ref(s.owner).tpe
    val self = new TypeWithContext(t, parentRefinements(t))
    addReachableType(self, null)
  }

  /** Builds the call graph based on the current reachable items, mainly entry points. */
  def build(): Unit = {
    ctx.log(s"\t Building call graph with ${reachableMethods.newItems.size} entry points")

    while (reachableMethods.hasNewItems || reachableTypes.hasNewItems || casts.hasNewItems) {
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
          val parent = new CallWithContext(x.tp.select(clas.primaryConstructor), x.tp.baseArgInfos(clas), Nil, x.outerTargs, null, null)
          reachableMethods ++= fields.map {
            fieldSym =>
              new CallWithContext(x.tp.select(fieldSym), Nil, Nil, x.outerTargs, parent, null)
          }
        }
      }

      val newReachableMethods = reachableMethods.newItems
      ctx.log(s"\t Found ${newReachableMethods.size} new call sites")

    }
  }

  /** Packages the current call graph into a CallGraph */
  def result(): CallGraph =
    CallGraph(entryPoints.toMap, reachableMethods.items, reachableTypes.items, casts.items, classOfs.items, outerMethods.toSet)

  private def addReachableType(x: TypeWithContext, from: CallWithContext): Unit = {
    if (!reachableTypes.contains(x)) {
      reachableTypes += x
      val namesInType = x.tp.memberNames(takeAllFilter).filter(typesByMemberNameCache.containsKey)
      for (name <- namesInType) {
        typesByMemberNameCache.put(name, typesByMemberNameCache.get(name) + x)
      }
    }
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
          tp.castsCache += newCast
        }
      }
    }
  }

  private def registerParentModules(tp: Type, from: CallWithContext): Unit = {
    var tp1 = tp
    while ((tp1 ne NoType) && (tp1 ne NoPrefix)) {
      if (tp1.widen ne tp1) registerParentModules(tp1.widen, from)
      if (tp1.dealias ne tp1) registerParentModules(tp1.dealias, from)
      if (tp1.termSymbol.is(Module)) {
        addReachableType(new TypeWithContext(tp1.widenDealias, parentRefinements(tp1.widenDealias)), from)
      } else if (tp1.typeSymbol.is(Module, Package)) {
        val t = ref(tp1.typeSymbol).tpe
        addReachableType(new TypeWithContext(t, parentRefinements(t)), from)
      }
      tp1 = tp1.normalizedPrefix
    }
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
        case _ =>
          foldOver(x, tp)
      }
    }.apply(OuterTargs.empty, tp)
  }

  private def instantiateCallSite(caller: CallWithContext, rec: Type, callee: CallInfo, types: Traversable[TypeWithContext]): Traversable[CallWithContext] = {

    val receiver = callee.call.normalizedPrefix
    registerParentModules(receiver, caller)

    val calleeSymbol = callee.call.termSymbol.asTerm
    val callerSymbol = caller.call.termSymbol

    val tpamsOuter = caller.call.widen match {
      case t: PolyType =>
        (t.paramNames zip caller.targs).foldLeft(OuterTargs.empty)((x, nameType) => x.add(callerSymbol, nameType._1, nameType._2))
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

    def dispatchCalls(receiverType: Type): Traversable[CallWithContext] = {
      receiverType match {
        case t: PreciseType =>
          def preciseSelectCall = {
            val selectByName = t.underlying.select(calleeSymbol.name)
            val call = selectByName.asInstanceOf[TermRef].denot.suchThat(x =>
              x == calleeSymbol || x.overriddenSymbol(calleeSymbol.owner.asClass) == calleeSymbol).symbol
            assert(call.exists)
            t.underlying.select(call)
          }
          new CallWithContext(preciseSelectCall, targs, args, outerTargs, caller, callee) :: Nil
        case t: ClosureType if calleeSymbol.name eq t.implementedMethod.name =>
          val methodSym = t.meth.meth.symbol.asTerm
          new CallWithContext(TermRef.withFixedSym(t.underlying, methodSym.name,  methodSym), targs, t.meth.env.map(_.tpe) ++ args, outerTargs ++ t.outerTargs, caller, callee) :: Nil
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
              new CallWithContext(tp.tp.select(alt.symbol), targs, args, outerTargs ++ tp.outerTargs, caller, callee)
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
              cast <- tp.castsCache
              if /*filterTypes(tp.tp, cast.from) &&*/ filterTypes(cast.to, receiverType) && filterCast(cast)
              alt <- tp.tp.member(calleeSymbol.name).altsWith(p => p.matches(calleeSymbol.asSeenFrom(tp.tp)))
              if alt.exists
            } yield {
              // this additionaly introduces a cast of result type and argument types
              val uncastedSig = tp.tp.select(alt.symbol).widen.appliedTo(targs).widen
              val castedSig = receiverType.select(calleeSymbol).widen.appliedTo(targs).widen
              (uncastedSig.paramTypess.flatten zip castedSig.paramTypess.flatten) foreach (x => addCast(x._2, x._1))
              addCast(uncastedSig.finalResultType, castedSig.finalResultType)

              new CallWithContext(tp.tp.select(alt.symbol), targs, args, outerTargs ++ tp.outerTargs, caller, callee)
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
        new CallWithContext(TermRef.withFixedSym(caller.call.normalizedPrefix, calleeSymbol.name, calleeSymbol), targs, args, outerTargs, caller, callee) :: Nil

      case t if calleeSymbol.isConstructor =>

        val constructedType = callee.call.widen.appliedTo(targs).widen.resultType
        val fixNoPrefix = if (constructedType.normalizedPrefix eq NoPrefix) {
          var currentPrefix = caller.call.normalizedPrefix
          while (!currentPrefix.classSymbol.exists) {
            if (currentPrefix.termSymbol.is(Module)) {
              currentPrefix = currentPrefix.widenDealias
            } else {
              currentPrefix = currentPrefix.normalizedPrefix
              currentPrefix = currentPrefix match {
                case t: ThisType => t.tref
                case _ => currentPrefix
              }
            }
          }
          constructedType match {
            case constructedType @ TypeRef(prefix, name)  =>
              constructedType.underlying match {
                case ci: ClassInfo =>
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
            propagateTargs(receiver).select(calleeSymbol)
        }

        new CallWithContext(call, targs, args, outerTargs, caller, callee) :: Nil

      // super call in a class (know target precisely)
      case st: SuperType =>
        val thisTpe = st.thistpe
        val targetClass = st.supertpe.baseClasses.find(clz =>
          clz.info.decl(calleeSymbol.name).altsWith(p => p.signature == calleeSymbol.signature).nonEmpty
        )
        val targetMethod = targetClass.get.info.member(calleeSymbol.name).altsWith(p => p.signature == calleeSymbol.signature).head.symbol.asTerm
        val thisTpePropagated = propagateTargs(thisTpe)


        new CallWithContext(TermRef.withFixedSym(thisTpePropagated, targetMethod.name, targetMethod), targs, args, outerTargs, caller, callee) :: Nil

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
              new CallWithContext(TermRef.withFixedSym(t, parentMethod.name, parentMethod), targs, args, outerTargs, caller, callee) :: Nil

            } else Nil
        }

      case thisType: ThisType if !calleeSymbol.owner.flags.is(PackageCreationFlags) =>
        val dropUntil = thisType.tref.classSymbol
        var currentThis = caller.call.normalizedPrefix
        var currentOwner = caller.call.termSymbol.owner
        while ((currentOwner ne dropUntil) && (currentThis ne NoType)) {
          if (!currentOwner.is(Method))
            currentThis = currentThis.normalizedPrefix
          currentOwner = currentOwner.owner.enclosingClass
        }
        if (currentThis.derivesFrom(thisType.cls)) {
          if (calleeSymbol.is(Private)) {
            new CallWithContext(TermRef.withFixedSym(currentThis, calleeSymbol.name, calleeSymbol), targs, args, outerTargs, caller, callee) :: Nil
          } else {
            val fullThisType = AndType.apply(currentThis, thisType.tref)
            dispatchCalls(propagateTargs(fullThisType))
          }
        } else {
          dispatchCalls(propagateTargs(receiver.widenDealias))
        }

      // todo: handle calls on this of outer classes


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

  private def processCallSites(callSites: immutable.Set[CallWithContext], instantiatedTypes: immutable.Set[TypeWithContext]): Unit = {
    for (method <- callSites) {
      // Find new call sites

      val sym = method.call.normalizedPrefix match {
        case t: ClosureType =>
          t.meth.meth.symbol
        case _ =>
          method.call.termSymbol
      }

      collectedSummaries.get(sym) match {
        case Some(summary) =>
          summary.accessedModules.foreach(x => addReachableType(new TypeWithContext(x.info, parentRefinements(x.info)), method))
          summary.methodsCalled.foreach {
            case (receiver, theseCallSites) => theseCallSites.foreach(callSite => processCallSite(callSite, instantiatedTypes, method, receiver))
          }

        case None =>
          outerMethods += sym

          def substituteOuterTargs = new SubstituteByParentMap(method.outerTargs)

          // Add return type to reachable types
          val returnType = method.call.widenDealias.finalResultType
          assert(!returnType.widenDealias.isInstanceOf[PolyType], returnType.widenDealias)
          val javaAllocatedType = returnType match {
            case returnType: JavaAllocatedType => returnType
            case returnType: HKApply => new JavaAllocatedType(substituteOuterTargs(returnType.tycon.appliedTo(method.targs)))
            case returnType => new JavaAllocatedType(substituteOuterTargs(returnType))
          }
          addReachableType(new TypeWithContext(javaAllocatedType, method.outerTargs), method)

          // Add all possible calls from java to object passed as parameters.
          processCallsFromJava(instantiatedTypes, method)
      }
    }
  }

  private def processCallsFromJava(instantiatedTypes: immutable.Set[TypeWithContext], method: CallWithContext): Unit = {
    def allNonJavaDecls(argType: Type) = {
      (argType.decls ++ argType.parents.filter(!_.symbol.is(JavaDefined)).flatMap(_.decls)).toSet
    }

    val addedTypes = mutable.HashSet.empty[Type]

    def addAllPotentialCallsFor(argType: Type): Unit = {
      if (!defn.isPrimitiveClass(argType.classSymbol) && !addedTypes.contains(argType)) {
        addedTypes.add(argType)
        for {
          decl <- allNonJavaDecls(argType)
          if decl.isTerm && !decl.isConstructor
          if decl.name != nme.isInstanceOf_ && decl.name != nme.asInstanceOf_ && decl.name != nme.synchronized_
        } {
          val termName = decl.name.asTermName
          val paramTypes = decl.info.paramTypess.flatten

          def addCall(call: TermRef): Unit = {
            val targs = call.widenDealias match {
              case call: PolyType => call.paramBounds.map(_.hi)
              case _ => Nil
            }
            processCallSite(CallInfo(call, targs, paramTypes), instantiatedTypes, method, argType)
            // TODO add transitively reachable calls from java (fix link-code-from-java-3.scala)
            // val resultType = call.widenDealias.finalResultType.widenDealias
            // addAllPotentialCallsFor(resultType)
          }

          val definedInJavaClass: Boolean = {
            def isDefinedInJavaClass(sym: Symbol) =
              sym.owner == defn.AnyClass || sym.owner.is(JavaDefined)

            isDefinedInJavaClass(decl) || decl.allOverriddenSymbols.exists(isDefinedInJavaClass)
          }

          argType match {
            case argType: PreciseType =>
              if (definedInJavaClass)
                addCall(new TermRefWithFixedSym(argType, termName, decl.asTerm))
            case _ =>
              // FIXME
//              val argTypeWiden = argType.widenDealias
//              if (argTypeWiden.member(termName).exists) {
//                val sym = argTypeWiden.classSymbol.requiredMethod(termName, paramTypes)
//                if (!definedInJavaClass || !sym.isEffectivelyFinal)
//                  addCall(TermRef(argType, sym))
//              }
          }
        }
      }
    }

    method.argumentsPassed.foreach(addAllPotentialCallsFor)
  }

  private def processCallSite(callSite: CallInfo, instantiatedTypes: immutable.Set[TypeWithContext], method: CallWithContext, receiver: Type): Unit = {
    val nw = instantiateCallSite(method, receiver, callSite, instantiatedTypes)
    reachableMethods ++= nw
    method.addOutEdges(callSite, nw)
  }

}
