package dotty.tools.dotc.transform.linker

import dotty.tools.dotc.ast.tpd
import dotty.tools.backend.jvm.CollectEntryPoints
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.NameOps._
import dotty.tools.dotc.core.TypeErasure
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.transform.ResolveSuper

import dotty.tools.dotc.transform.linker.Summaries._
import dotty.tools.dotc.transform.linker.CollectSummaries.SubstituteByParentMap

import collection.{immutable, mutable}

object BuildCallGraph {
  final val AnalyseOrig = 1
  final val AnalyseTypes = 2
  final val AnalyseArgs = 3
}

class BuildCallGraph extends Phase {
  import BuildCallGraph._

  private var callGraph: CallGraph = _

  def getCallGraph: CallGraph = callGraph

  import tpd._
  def phaseName: String = "callGraph"
  def isEntryPoint(s: Symbol)(implicit ctx: Context): Boolean = {
    ((s.name eq nme.main) /* for speed */  && s.is(Method) && CollectEntryPoints.isJavaMainMethod(s)) || // Java main method
    (s.is(Method) && s.hasAnnotation(defn.ExportAnnot)) // Explicit entry point
  }

  def parentRefinements(tp: Type)(implicit ctx: Context): OuterTargs =
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

  /**
    * @param mode see modes above
    * @param specLimit how many specializations symbol can have max
    * @return (reachableMethods, reachableTypes, casts, outerMethod)
    */
  private def buildCallGraph(mode: Int, specLimit: Int)(implicit ctx: Context): CallGraph = {
    val startTime = java.lang.System.currentTimeMillis()
    val collectedSummaries = ctx.summariesPhase.asInstanceOf[CollectSummaries].methodSummaries.map(x => (x.methodDef, x)).toMap
    val reachableMethods = new WorkList[CallWithContext]()
    val reachableTypes = new WorkList[TypeWithContext]()
    val casts = new WorkList[Cast]()
    val classOfs = new WorkList[Symbol]()
    val outerMethods = mutable.Set[Symbol]()
    val typesByMemberNameCache = new java.util.IdentityHashMap[Name, Set[TypeWithContext]]()

    def getTypesByMemberName(x: Name): Set[TypeWithContext] = {
      val ret1 = typesByMemberNameCache.get(x)
      if (ret1 eq null) {
        // not yet computed
        val upd = reachableTypes.items.filter(tp => tp.tp.member(x).exists)
        typesByMemberNameCache.put(x, upd)
        upd
      } else ret1
    }
    def addReachableType(x: TypeWithContext, from: CallWithContext): Unit = {
      if (!reachableTypes.contains(x)) {
        reachableTypes += x
        val namesInType = x.tp.memberNames(takeAllFilter).filter(typesByMemberNameCache.containsKey)
        for (name <- namesInType) {
            typesByMemberNameCache.put(name, typesByMemberNameCache.get(name) + x)
        }
      }
    }

    def pushEntryPoint(s: Symbol) = {
      val tpe = ref(s).tpe
      val targs = tpe.widen match {
        case t: PolyType => t.paramNames.size
        case _ => 0
      }
      val call = new CallWithContext(tpe, (0 until targs).map(x => new ErazedType()).toList, ctx.definitions.ArrayOf(ctx.definitions.StringType) :: Nil, OuterTargs.empty, null, null)
      reachableMethods += call
      val t = ref(s.owner).tpe
      val self = new TypeWithContext(t, parentRefinements(t))
      addReachableType(self, null)
    }

    collectedSummaries.values.foreach { x =>
      if (isEntryPoint(x.methodDef)) {
        pushEntryPoint(x.methodDef)
        // FIXME: add the lazy val as an entry point for the initialization module containing the entry point.
        // pushEntryPoint(x.methodDef.owner.asClass.sourceModule)
      }
    }
    println(s"\t Found ${reachableMethods.newItems.size} entry points")

    def registerParentModules(tp: Type, from: CallWithContext): Unit = {
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

    def instantiateCallSite(caller: CallWithContext, rec: Type, callee: CallInfo, types: Traversable[TypeWithContext]): Traversable[CallWithContext] = {

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
          ref(Summaries.simplifiedClassOf(x)).tpe
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

      def addCast(from: Type, to: Type) =
        if (!(from <:< to) && to.classSymbols.forall(!_.derivesFrom(defn.NothingClass))) {
          val newCast = Cast(from, to)

          for (tp <- reachableTypes.items) {
            if (from.classSymbols.forall(x => tp.tp.classSymbols.exists(y => y.derivesFrom(x))) && to.classSymbols.forall(x => tp.tp.classSymbols.exists(y => y.derivesFrom(x)))) {
              casts += newCast
              tp.castsCache += newCast
            }
          }
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
            new CallWithContext(t.underlying.select(calleeSymbol.name), targs, args, outerTargs, caller, callee) :: Nil
          case t: ClosureType if calleeSymbol.name eq t.implementedMethod.name =>
            val methodSym = t.meth.meth.symbol.asTerm
            new CallWithContext(TermRef.withFixedSym(t.underlying, methodSym.name,  methodSym), targs, t.meth.env.map(_.tpe) ++ args, outerTargs ++ t.outerTargs, caller, callee) :: Nil
          case _ =>
            // without casts
            val direct =
              for (tp <- getTypesByMemberName(calleeSymbol.name)
                   if filterTypes(tp.tp, receiverType.widenDealias);
                   alt <- tp.tp.member(calleeSymbol.name).altsWith(p => p.asSeenFrom(tp.tp).matches(calleeSymbol.asSeenFrom(tp.tp)))
                   if alt.exists
              )
                yield new CallWithContext(tp.tp.select(alt.symbol), targs, args, outerTargs ++ tp.outerTargs, caller, callee)

            val casted = if (mode < AnalyseTypes) Nil
            else
              for (tp <- getTypesByMemberName(calleeSymbol.name);
                   cast <- tp.castsCache
                   if /*filterTypes(tp.tp, cast.from) &&*/ filterTypes(cast.to, receiverType) && {
                     val receiverBases = receiverType.classSymbols
                     val targetBases = cast.to.classSymbols
                     receiverBases.forall(c => targetBases.exists(_.derivesFrom(c)))
                     //cast.to.classSymbol != defn.NothingClass
                   };
                   alt <- tp.tp.member(calleeSymbol.name).altsWith(p => p.matches(calleeSymbol.asSeenFrom(tp.tp)))
                   if alt.exists && {
                     // this additionaly introduces a cast of result type and argument types

                     val uncastedSig = tp.tp.select(alt.symbol).widen.appliedTo(targs).widen
                     val castedSig = receiverType.select(calleeSymbol).widen.appliedTo(targs).widen
                     (uncastedSig.paramTypess.flatten zip castedSig.paramTypess.flatten) foreach (x => addCast(x._2, x._1))
                     addCast(uncastedSig.finalResultType, castedSig.finalResultType)

                     true
                   })
                yield new CallWithContext(tp.tp.select(alt.symbol), targs, args, outerTargs ++ tp.outerTargs, caller, callee)

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
          assert(callee.call.termSymbol.owner.is(Method) || callee.call.termSymbol.owner.isLocalDummy)
          new CallWithContext(TermRef.withFixedSym(caller.call.normalizedPrefix, calleeSymbol.name, calleeSymbol), targs, args, outerTargs, caller, callee) :: Nil

        case t if calleeSymbol.isConstructor =>

          val constructedType = callee.call.widen.appliedTo(targs).widen.resultType
          val fixNoPrefix = if (constructedType.normalizedPrefix eq NoPrefix) {
            var currentPrefix = caller.call.normalizedPrefix
            while (!currentPrefix.classSymbol.exists) {
              currentPrefix = currentPrefix.normalizedPrefix
              currentPrefix = currentPrefix match {
                case t: ThisType =>
                  t.tref

                case _ => currentPrefix
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
            currentThis = currentThis.normalizedPrefix
            currentOwner = currentOwner.owner.enclosingClass
          }
          if (currentThis.derivesFrom(thisType.cls)) {
            val fullThisType = AndType.apply(currentThis, thisType.tref)
            if (calleeSymbol.is(Private))
              new CallWithContext(TermRef.withFixedSym(currentThis, calleeSymbol.name, calleeSymbol), targs, args, outerTargs, caller, callee) :: Nil
            else dispatchCalls(propagateTargs(fullThisType))
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

    def processCallSites(callSites: immutable.Set[CallWithContext], instantiatedTypes: immutable.Set[TypeWithContext]): Unit = {

      for (method <- callSites) {
        // Find new call sites

        val sym = method.call.normalizedPrefix match {
          case t: ClosureType =>
            t.meth.meth.symbol
          case _ =>
            method.call.termSymbol
        }


        def processCallSite(callSite: CallInfo, receiver: Type): Unit = {
          val nw = instantiateCallSite(method, receiver, callSite, instantiatedTypes)
          method.outEdges(callSite) = nw.filter(x => !method.outEdges(callSite).contains(x)).toList ::: method.outEdges(callSite)
          reachableMethods ++= nw
        }

        def processCallsFromJava(): Unit = {
          for {
            (paramType, argType) <- method.call.widenDealias.paramTypess.flatten.zip(method.argumentsPassed)
            if !defn.isPrimitiveClass(paramType.classSymbol)
            decl <- paramType.decls
            if decl.isTerm && !decl.isConstructor
            if decl.name != nme.isInstanceOf_ && decl.name != nme.asInstanceOf_ && decl.name != nme.synchronized_
          } yield {
            val termName = decl.name.asTermName
            val paramTypes = decl.info.paramTypess.flatten

            def addCall(call: TermRef): Unit = {
              val targs = call.widenDealias match {
                case call: PolyType => call.paramBounds.map(_.hi)
                case _ => Nil
              }
              processCallSite(CallInfo(call, targs, paramTypes), argType)
            }

            argType match {
              case argType: PreciseType =>
                val sym = argType.underlying.classSymbol.requiredMethod(termName, paramTypes)
                if (sym.owner != defn.AnyClass && !sym.owner.is(JavaDefined))
                  addCall(new TermRefWithFixedSym(argType, termName, sym))
              case _ =>
                val sym = argType.widenDealias.classSymbol.requiredMethod(termName, paramTypes)
                if (!(sym.owner.is(JavaDefined) && (sym.is(Final) || sym.owner.is(Final))))
                  addCall(TermRef(argType, sym))
            }
          }
        }

        collectedSummaries.get(sym) match {
          case Some(summary) =>
            summary.accessedModules.foreach(x => addReachableType(new TypeWithContext(x.info, parentRefinements(x.info)), method))
            summary.methodsCalled.foreach {
              case (receiver, theseCallSites) => theseCallSites.foreach(callSite => processCallSite(callSite, receiver))
            }

          case None =>
            outerMethods += sym

            // Add return type to reachable types
            addReachableType(new TypeWithContext(new JavaAllocatedType(method.call.widenDealias.finalResultType), OuterTargs.empty), method)

            // Add all possible calls from java to object passed as parameters.
            processCallsFromJava()
        }
      }

    }


    while (reachableMethods.hasNewItems || reachableTypes.hasNewItems || casts.hasNewItems) {
      reachableMethods.clearNewItems()
      reachableTypes.clearNewItems()
      casts.clearNewItems()
      classOfs.clearNewItems()

      processCallSites(reachableMethods.items, reachableTypes.items)

      val newReachableTypes = reachableTypes.newItems
      println(s"\t Found ${newReachableTypes.size} new instantiated types: " + newReachableTypes.take(10).map(_.tp.show).mkString("Set(", ", ", if (newReachableTypes.size <= 10) ")" else ", ...)"))
      val newClassOfs = classOfs.newItems
      println(s"\t Found ${newClassOfs.size} new classOfs: " + newClassOfs.take(10).map(_.show).mkString("Set(", ", ", if (newClassOfs.size <= 10) ")" else ", ...)"))
      newReachableTypes.foreach { x =>
        val clas = x.tp match {
          case t: ClosureType =>
            t.u.classSymbol.asClass
          case t: JavaAllocatedType =>
            t.underlying.classSymbol.asClass
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
      println(s"\t Found ${newReachableMethods.size} new call sites: " + newReachableMethods.take(10).map(x => { val sym = x.call.termSymbol; (sym.owner, sym) }).mkString("Set(", ", ", if (newReachableMethods.size <= 10) ")" else ", ...)"))

    }

    val endTime = java.lang.System.currentTimeMillis()
    println("++++++++++ finished in " + (endTime - startTime)/1000.0  +" seconds. ++++++++++ ")

    CallGraph(reachableMethods.items, reachableTypes.items, casts.items, classOfs.items, outerMethods.toSet)
  }

  def sendSpecializationRequests(callGraph: CallGraph)(implicit ctx: Context): Unit = {
//   ctx.outerSpecPhase match {
//      case specPhase: OuterSpecializer =>
//
//        callGraph.reachableMethods.foreach { mc =>
//          val methodSym = mc.call.termSymbol
//          val outerTargs = methodSym.info.widen match {
//            case PolyType(names) =>
//              (names zip mc.targs).foldLeft(mc.outerTargs)((x, nameType) => x.+(methodSym, nameType._1, nameType._2))
//            case _ =>
//              mc.outerTargs
//          }
//          if (outerTargs.mp.nonEmpty && !methodSym.isPrimaryConstructor)
//            specPhase.registerSpecializationRequest(methodSym)(outerTargs)
//        }
//        callGraph.reachableTypes.foreach { tpc =>
//          val parentOverrides = tpc.tp.typeMembers(ctx).foldLeft(OuterTargs.empty)((outerTargs, denot) =>
//            denot.symbol.allOverriddenSymbols.foldLeft(outerTargs)((outerTargs, sym) =>
//              outerTargs.+(sym.owner, denot.symbol.name, denot.info)))
//
//          val spec = tpc.outerTargs ++ parentOverrides ++ parentRefinements(tpc.tp)
//
//          if (spec.nonEmpty) {
//            specPhase.registerSpecializationRequest(tpc.tp.typeSymbol)(spec)
//            def loop(remaining: List[Symbol]): Unit = {
//              if (remaining.isEmpty) return;
//              val target = remaining.head
//
//              val nspec = OuterTargs(spec.mp.filter{x => target.derivesFrom(x._1)})
//              if (nspec.nonEmpty)
//                specPhase.registerSpecializationRequest(target)(nspec)
//              loop(remaining.tail)
//            }
//            val parents = tpc.tp.baseClasses
//            loop(parents)
//          }
//        }
//      case _ =>
//       ctx.warning("No specializer phase found")
//    }

  }

  private var runOnce = true
  def run(implicit ctx: Context): Unit = {
    if (runOnce /*&& ctx.settings.lto.value.nonEmpty*/) {
      val specLimit = 15
      //println(s"\n\t\t\tOriginal analisys")
      //val g1 = buildCallGraph(AnalyseOrig, specLimit)

      //println(s"\n\t\t\tType flow analisys")
      //val g2 = buildCallGraph(AnalyseTypes, specLimit)

      println(s"\n\t\t\tType & Arg flow analisys")
      val callGraph = buildCallGraph(AnalyseArgs, specLimit)
      this.callGraph = callGraph

      val g3 = GraphVisualization.outputGraph(AnalyseArgs, specLimit)(callGraph)

      sendSpecializationRequests(callGraph)

      def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit): Unit = {
        val p = new java.io.PrintWriter(f)
        try { op(p) } finally { p.close() }
      }

      //printToFile(new java.io.File("out1.dot")) { out =>
      //  out.println(g1)
      //}
     // printToFile(new java.io.File("out2.dot")) { out =>
     //   out.println(g2)
     // }
      printToFile(new java.io.File("CallGraph.dot")) { out =>
        out.println(g3)
      }

    }
    runOnce = false
  }
}
