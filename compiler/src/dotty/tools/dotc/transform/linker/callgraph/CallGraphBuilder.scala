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

  private var entryPoints = immutable.Map.empty[CallInfoWithContext, Int]
  private var reachableTypes = immutable.Set.empty[TypeWithContext]
  private var reachableMethods = mutable.Set.empty[CallInfoWithContext]

  private var reachableMethodsSymbols = immutable.Set.empty[Symbol]
  private var outerMethods = immutable.Set.empty[Symbol]

  private var classOfs = immutable.Set.empty[Symbol]

  private val typesByMemberNameCache = new java.util.IdentityHashMap[Name, List[(Int, TypeWithContext)]]()

  private var casts = immutable.Set.empty[Cast]
  private val castsCache: mutable.HashMap[TypeWithContext, mutable.Set[Cast]] = mutable.HashMap.empty

  private val sizesCache = new mutable.HashMap[Symbol, Int]()
  private val lastInstantiation = mutable.Map.empty[CallInfoWithContext, mutable.Map[CallInfo, Int]]
  private var finished = false

  private val normalizeType: Type => Type = new TypeNormalizer

  def pushEntryPoint(s: Symbol, entryPointId: Int): Unit = {
    val tpe = ref(s).tpe.asInstanceOf[TermRef]
    val targsSize = tpe.widen match {
      case t: PolyType => t.paramNames.size
      case _ => 0
    }
    val targs = (0 until targsSize).map(x => new ErazedType()).toList
    val args = ctx.definitions.ArrayOf(ctx.definitions.StringType) :: Nil
    val call = CallInfoWithContext(tpe, targs, args, OuterTargs.empty, None, None)
    entryPoints = entryPoints.updated(call, entryPointId)
    addReachableMethod(call)
    val t = normalizeType(ref(s.owner).tpe)
    val self = new TypeWithContext(t, parentRefinements(t))
    addReachableType(self)
  }

  /** Builds the call graph based on the current reachable items, mainly entry points. */
  def build(): Unit = {
    ctx.log(s"Building call graph with ${entryPoints.values.toSet.size} entry points")
    val startTime = System.currentTimeMillis()
    @tailrec def buildLoop(): Unit = {
      val loopStartTime = System.currentTimeMillis()

      val reachableMethodsLastSize = reachableMethods.size
      val reachableTypesLastSize = reachableTypes.size
      val castsLastSize = casts.size
      val classOfsLastSize = classOfs.size

      processCallSites()

      val numNewReachableMethods = reachableMethods.size - reachableMethodsLastSize
      val numNewReachableTypes = reachableTypes.size - reachableTypesLastSize
      val numNewCasts = casts.size - castsLastSize
      val numNewClassOfs = classOfs.size - classOfsLastSize

      iteration += 1
      val loopEndTime = System.currentTimeMillis()
      val loopTime = loopEndTime - loopStartTime
      val totalTime = loopEndTime - startTime
      ctx.log(
        s"""Graph building iteration $iteration
           |Iteration in ${loopTime/1000.0} seconds out of ${totalTime/1000.0} seconds (${loopTime.toDouble/totalTime})
           |Found $numNewReachableTypes new instantiated types (${reachableTypes.size})
           |Found $numNewReachableMethods new call sites (${reachableMethods.size})
           |Found $numNewCasts new casts (${casts.size})
           |Found $numNewClassOfs new classOfs (${classOfs.size})""".stripMargin)

      // val outFile =  new java.io.File(ctx.settings.d.value + s"/CallGraph-${reachableMethods.items.size}.html")
      // GraphVisualization.outputGraphVisToFile(this.result(), outFile)

      if (numNewReachableMethods != 0 || numNewReachableTypes != 0 || numNewCasts != 0)
        buildLoop()
      else if (!finished) {
        // This last loop is only here to check the correctness on the crc map
        // TODO: remove in production mode
        ctx.log("[processing all call sites for crc check]")
        finished = true
        buildLoop()
      }
    }

    buildLoop()
  }

  /** Packages the current call graph into a CallGraph */
  def result(): CallGraph = {
    val entryPoints = this.entryPoints
    val reachableMethods = this.reachableMethods.toSet
    val reachableTypes = this.reachableTypes
    val casts = this.casts
    val classOfs = this.classOfs
    val outerMethods = this.outerMethods
    new CallGraph(entryPoints, reachableMethods, reachableTypes, casts, classOfs, outerMethods, mode, specLimit)
  }


  private def addReachableType(x: TypeWithContext): Unit = {
    if (!reachableTypes.contains(x)) {
      finished = false // TODO: replace with assert(!finished)

      val substed = new SubstituteByParentMap(x.outerTargs).apply(x.tp)
      def registerSize(sym: Symbol): Unit = sizesCache(sym) = sizesCache.getOrElse(sym, 0) + 1

      val classSymbols = mutable.Set.empty[Symbol]
      def collectClasses(cls: ClassSymbol): Unit = {
        classSymbols.add(cls)
        cls.classParents.foreach(x => collectClasses(x.symbol.asClass))
      }
      substed.classSymbols.foreach(collectClasses)
      classSymbols.foreach(registerSize)

      reachableTypes += x

      lazy val deepness = TypeDepth(x.tp)
      val namesInType = x.tp.memberNames(takeAllFilter).filter(typesByMemberNameCache.containsKey)
      for (name <- namesInType) {
        typesByMemberNameCache.put(name, (deepness, x) :: typesByMemberNameCache.get(name))
      }

      val clas = x.tp match {
        case t: ClosureType => t.u.classSymbol.asClass
        case t: JavaAllocatedType => t.underlying.widenDealias.classSymbol.asClass
        case _ => x.tp.classSymbol.asClass
      }
      if (!clas.is(JavaDefined) && clas.is(Module)) {
        val fields = clas.classInfo.decls.filter(x => !x.is(Method) && !x.isType)
        val parent = Some(CallInfoWithContext(x.tp.select(clas.primaryConstructor).asInstanceOf[TermRef], x.tp.baseArgInfos(clas), Nil, x.outerTargs, None, None))
        fields.foreach { fieldSym =>
          addReachableMethod(CallInfoWithContext(x.tp.select(fieldSym).asInstanceOf[TermRef], Nil, Nil, x.outerTargs, parent, None))
        }
      }
    }
  }

  private def addReachableMethod(method: CallInfoWithContext): Unit = {
    if (!reachableMethods.contains(method)) {
      finished = false // TODO: replace with assert(!finished)
      reachableMethods += method
      val callSymbol = method.callSymbol
      if (!reachableMethodsSymbols.contains(callSymbol)) {
        reachableMethodsSymbols += callSymbol
        collectedSummaries.get(callSymbol).foreach { summary =>
          summary.accessedModules.foreach(x => addReachableType(new TypeWithContext(normalizeType(x.info), parentRefinements(x.info))))
        }
      }
    }
  }

  private def addReachableClosure(x: ClosureType, from: CallInfoWithContext): Unit = {
    val substitution = new SubstituteByParentMap(from.outerTargs)
    val tp = substitution(x)
    addReachableType(new TypeWithContext(tp, parentRefinements(tp)))
  }

  private def getTypesByMemberName(x: Name): List[(Int, TypeWithContext)] = {
    val ret1 = typesByMemberNameCache.get(x)
    if (ret1 eq null) {
      // not yet computed
      val upd = reachableTypes.iterator.collect { case tp if tp.tp.member(x).exists => (TypeDepth(tp.tp), tp) }.toList
      typesByMemberNameCache.put(x, upd)
      upd
    } else ret1
  }

  private def addCast(from: Type, to: Type) = {
    if (!(from <:< to) && to.classSymbols.forall(!_.derivesFrom(defn.NothingClass))) {
      val newCast = new Cast(from, to)
      var addedCast = false
      val classSymbols = from.classSymbols.toSet ++ to.classSymbols
      for (tp <- reachableTypes) {
        if (classSymbols.forall(x => tp.tp.classSymbols.exists(y => y.derivesFrom(x)))) {
          val cached = castsCache.getOrElseUpdate(tp, mutable.Set.empty)
          if (!cached.contains(newCast)) {
            if (!addedCast) {
              finished = false // TODO: replace with assert(!finished)
              casts += newCast
              addedCast = true
            }
            cached += newCast
          }
        }
      }
    }
  }

  private val registeredParentModules = mutable.Set.empty[Type]
  private def registerParentModules(tp: Type): Unit = {
    if ((tp ne NoType) && (tp ne NoPrefix) && !registeredParentModules.contains(tp)) {
      if (tp.widen ne tp) registerParentModules(tp.widen)
      if (tp.dealias ne tp) registerParentModules(tp.dealias)
      if (tp.termSymbol.is(Module)) {
        addReachableType(new TypeWithContext(tp.widenDealias, parentRefinements(tp.widenDealias)))
      } else if (tp.typeSymbol.is(Module, butNot = Package)) {
        val t = normalizeType(ref(tp.typeSymbol).tpe)
        addReachableType(new TypeWithContext(t, parentRefinements(t)))
      }
      registeredParentModules += tp
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

  private def instantiateCallSite(caller: CallInfoWithContext, callee: CallInfo, registerCall: CallInfoWithContext => Boolean): Unit = {

    def addCall(call: CallInfoWithContext) = {
      if (registerCall(call)) {
        addReachableMethod(call)
        caller.addOutEdges(callee, call)
      }
    }

    lazy val someCaller = Some(caller)
    lazy val someCallee = Some(callee)

    val receiver = callee.call.normalizedPrefix

    registerParentModules(receiver)

    lazy val receiverDepth = TypeDepth(receiver)

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
        val t  = parentRefinements(propagateTargs(receiver)) ++ caller.outerTargs
        val (a, b) = t.mp.partition(x => calleeSymbol.isProperlyContainedIn(x._1))
        new OuterTargs(a) combine new OuterTargs(b)
      }

    // if typearg of callee is a typeparam of caller, propagate typearg from caller to callee
    lazy val targs = callee.targs map {
      case t: TypeVar if mode >= AnalyseTypes && t.stripTypeVar.typeSymbol.maybeOwner == caller.call.termSymbol =>
        assert(caller.call.termSymbol.exists)
        val abstractSym = t.stripTypeVar.typeSymbol
        val id = caller.call.termSymbol.info.asInstanceOf[PolyType].paramNames.indexOf(abstractSym.name)
        propagateTargs(caller.targs(id).stripTypeVar)
      case t if mode >= AnalyseTypes => propagateTargs(t.stripTypeVar)
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
        val utpe =  normalizeType(propagateTargs(x.underlying, isConstructor = true))
        val outer = parentRefinements(utpe) ++ caller.outerTargs
        val closureT = new ClosureType(x.meth, utpe, x.implementedMethod)
        addReachableType(new TypeWithContext(closureT, outer))
        closureT
      case x: TermRef if x.symbol.is(Param) && x.symbol.owner == caller.call.termSymbol =>
        val id = caller.call.termSymbol.info.paramNamess.flatten.indexWhere(_ == x.symbol.name)
        caller.argumentsPassed(id)
      case x => propagateTargs(x)
    }

    def filterTypes(tp1: Type, tp2: Type, dtp1: Int, dtp2: Int): Boolean = {
      if (dtp1 < dtp2) false
      else if (mode >= AnalyseTypes) tp1 <:< tp2
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

    def dispatchCalls(receiverType: Type, dispatch: CallInfoWithContext => Unit = addCall): Unit = {
      receiverType match {
        case t: PreciseType =>
          dispatch(CallInfoWithContext(preciseSelectCall(t.underlying, calleeSymbol), targs, args, outerTargs, someCaller, someCallee))
        case t: ClosureType if calleeSymbol.name eq t.implementedMethod.name =>
          val methodSym = t.meth.meth.symbol.asTerm
          dispatch(CallInfoWithContext(TermRef.withFixedSym(t.underlying, methodSym.name,  methodSym), targs, t.meth.env.map(_.tpe) ++ args, outerTargs, someCaller, someCallee))
        case AndType(tp1, tp2) =>
          val set1 = mutable.Set.empty[CallInfoWithContext]
          val set2 = mutable.Set.empty[CallInfoWithContext]
          dispatchCalls(tp1, x => set1.add(x))
          dispatchCalls(tp2, x => set2.add(x))
          set1.intersect(set2).foreach(dispatch)

        case _ =>
          // without casts
          val receiverTypeWiden = receiverType.widenDealias match {
            case t: TypeRefWithFixedSym if t.classSymbol.owner.is(Method) =>
              new TypeRefWithFixedSym(t.classSymbol.owner.owner.typeRef, t.name, t.fixedSym)
            case t => t
          }

          def denotMatch(tp: TypeWithContext, p: Symbol) = {
            val d1 = p.asSeenFrom(tp.tp)
            val d2 = calleeSymbol.asSeenFrom(tp.tp)
            (d1 eq d2) || d1.matches(d2)
          }


          for {
            (dtp, tp) <- getTypesByMemberName(calleeSymbol.name)
            if filterTypes(tp.tp, receiverTypeWiden, dtp, receiverDepth)
            alt <- tp.tp.member(calleeSymbol.name).altsWith(p => denotMatch(tp, p))
            if alt.exists
          } {
            dispatch(CallInfoWithContext(tp.tp.select(alt.symbol).asInstanceOf[TermRef], targs, args, outerTargs ++ tp.outerTargs, someCaller, someCallee))
          }

          if (mode >= AnalyseTypes) {
            def filterCast(cast: Cast) = {
              val receiverBases = receiverType.classSymbols
              val targetBases = cast.to.classSymbols
              receiverBases.forall(c => targetBases.exists(_.derivesFrom(c)))
            }
            for {
              (dtp, tp) <- getTypesByMemberName(calleeSymbol.name)
              cast <- castsCache.getOrElse(tp, Iterator.empty)
              if filterCast(cast) && filterTypes(tp.tp, cast.from, dtp, cast.fromDepth) && filterTypes(cast.to, receiverType, cast.toDepth, receiverDepth)
              alt <- tp.tp.member(calleeSymbol.name).altsWith(p => p.matches(calleeSymbol.asSeenFrom(tp.tp)))
              if alt.exists
            } {
              // this additionally introduces a cast of result type and argument types
              val uncastedSig = preciseSelectCall(tp.tp, alt.symbol).widen.appliedTo(targs).widen
              val castedSig = preciseSelectCall(receiverType, calleeSymbol).widen.appliedTo(targs).widen
              (uncastedSig.paramTypess.flatten zip castedSig.paramTypess.flatten) foreach (x => addCast(x._2, x._1))
              addCast(uncastedSig.finalResultType, castedSig.finalResultType) // FIXME: this is added even in and tpe that are out of the intersection

              dispatch(CallInfoWithContext(tp.tp.select(alt.symbol).asInstanceOf[TermRef], targs, args, outerTargs ++ tp.outerTargs, someCaller, someCallee))
            }
          }
      }
    }

    receiver match {
      case _ if calleeSymbol == defn.Predef_classOf =>
        classOfs += callee.targs.head.classSymbol
      case _ if calleeSymbol == ctx.definitions.throwMethod =>
      case _ if calleeSymbol == ctx.definitions.Any_asInstanceOf =>
        val from = propagateTargs(receiver)
        val to = propagateTargs(targs.head)
        addCast(from, to)
      case _ if defn.ObjectMethods.contains(calleeSymbol) || defn.AnyMethods.contains(calleeSymbol) => Nil
      case NoPrefix =>  // inner method
        assert(calleeSymbol.is(ParamAccessor) || calleeSymbol.owner.is(Method) || calleeSymbol.owner.isLocalDummy)
        addCall(CallInfoWithContext(TermRef.withFixedSym(caller.call.normalizedPrefix, calleeSymbol.name, calleeSymbol), targs, args, outerTargs, someCaller, someCallee))

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

        val tpe = propagateTargs(fixNoPrefix, isConstructor = true)
        addReachableType(new TypeWithContext(tpe, parentRefinements(tpe) ++ outerTargs))

        val call = {
          if (callerSymbol.isConstructor && callerSymbol.owner == calleeSymbol.owner)
            new TermRefWithFixedSym(calleeSymbol.owner.typeRef, calleeSymbol.name, calleeSymbol)
          else
            propagateTargs(receiver).select(calleeSymbol).asInstanceOf[TermRef]
        }

        addCall(CallInfoWithContext(call, targs, args, outerTargs, someCaller, someCallee))

      // super call in a class (know target precisely)
      case st: SuperType =>
        val thisTpe = st.thistpe
        val targetClass = st.supertpe.baseClasses.find(clz =>
          clz.info.decl(calleeSymbol.name).altsWith(p => p.signature == calleeSymbol.signature).nonEmpty
        )
        val targetMethod = targetClass.get.info.member(calleeSymbol.name).altsWith(p => p.signature == calleeSymbol.signature).head.symbol.asTerm
        val thisTpePropagated = propagateTargs(thisTpe)


        addCall(CallInfoWithContext(TermRef.withFixedSym(thisTpePropagated, targetMethod.name, targetMethod), targs, args, outerTargs, someCaller, someCallee))

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
        getTypesByMemberName(memberName).foreach {
          x =>
            val s = x._2.tp.baseClasses.dropWhile(_ != prev)
            if (s.nonEmpty) {
              val parentMethod = ResolveSuper.rebindSuper(x._2.tp.widenDealias.classSymbol, calleeSymbol).asTerm
              // todo: outerTargs are here defined in terms of location of the subclass. Is this correct?
              addCall(CallInfoWithContext(TermRef.withFixedSym(t, parentMethod.name, parentMethod), targs, args, outerTargs, someCaller, someCallee))

            }
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
          addCall(CallInfoWithContext(TermRef.withFixedSym(currentThis, calleeSymbol.name, calleeSymbol), targs, args, outerTargs, someCaller, someCallee))
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
    var processed = 0
    var total = 0

    def computeCRC(tp: Type) = tp.classSymbols.iterator.map(x => sizesCache.getOrElse(x, 0)).sum

    for (method <- reachableMethods) {
      lazy val crcMap = lastInstantiation.getOrElseUpdate(method, mutable.Map.empty)
      // Find new call sites

      def needsCallSiteInstantiation(callSite: CallInfo): Boolean = {
        total += 1
        val receiver = callSite.call.normalizedPrefix
        val recomputedCRC = computeCRC(receiver)
        val needsInstantiation = recomputedCRC != crcMap.getOrElse(callSite, -1)
        if (needsInstantiation)
          crcMap(callSite) = recomputedCRC
        needsInstantiation || finished // if finished==true we are checking the completeness of the CRC
      }

      collectedSummaries.get(method.callSymbol) match {
        case Some(summary) =>
          summary.definedClosures.foreach(x => addReachableClosure(x, method))

          for {
            methodCalled <- summary.methodsCalled
            callSite <- methodCalled._2
            if needsCallSiteInstantiation(callSite)
          } {
            processed += 1
            instantiateCallSite(method, callSite, _ => true)
          }

        case None =>

          // val loadedSummary = ctx.summariesPhase.asInstanceOf[CollectSummaries].getLoadedSummary(method.callSymbol)

          if (!outerMethods.contains(method.callSymbol)) {

            outerMethods += method.callSymbol

            if (withJavaCallGraph && !method.callSymbol.is(Module | Package) && !method.parent.exists(_.isOnJavaAllocatedType)) {

              addReachableJavaReturnType(method)

              // Add all possible calls from java to object passed as parameters.
              for {
                rec <- (method.call.normalizedPrefix :: method.argumentsPassed).distinct
                potentialCall <- allPotentialCallsFor(rec)
                if needsCallSiteInstantiation(potentialCall) && method.getOutEdges(potentialCall).isEmpty
              } {
                processed += 1
                instantiateCallSite(method, potentialCall, call => collectedSummaries.contains(call.callSymbol))
              }
            }
          }
      }
    }

    ctx.log(s"Processed $processed calls out of $total")
  }

  private def addReachableJavaReturnType(method: CallInfoWithContext): Unit = {
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
  }

  private def allPotentialCallsFor(argType: Type): Set[CallInfo] = {
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

}
