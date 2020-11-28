package dotty.tools.dotc
package transform
package init

import scala.collection.mutable

import core._
import Contexts._
import ast.tpd._
import Decorators._
import Symbols._
import Constants.Constant
import Types._
import util.NoSourcePosition
import reporting.trace
import config.Printers.init

import Effects._, Potentials._, Summary._, Util._, Errors._

object Checking {
  /** The checking state
   *
   *  Why `visited` is a set of effects instead of `Symbol`? Think the following program:
   *
   *      class C(x: Int, a: A @cold) {
   *        val n = if (x > 0) new C(x - 1, a).m() else 0
   *        val b: Int = this.m()
   *        def m(): Int = b
   *      }
   *
   */
  case class State(
    private var visited: Set[Effect],          // effects that have been checked
    path: Vector[Tree],                        // the path that leads to the current effect
    thisClass: ClassSymbol,                    // the concrete class of `this`
    fieldsInited: mutable.Set[Symbol],
    parentsInited: mutable.Set[ClassSymbol],
    env: Env,
    var superCallFinished: Boolean = false,                       // whether super call has finished, used in singleton objects
    safePromoted: mutable.Set[Potential] = mutable.Set.empty      // potentials that can be safely promoted
  ) {

    def isGlobalObject(using Context): Boolean = thisClass.is(Flags.Module) && thisClass.isStatic

    def withVisited(eff: Effect): State = {
      visited = visited + eff
      copy(path = this.path :+ eff.source)
    }

    def hasVisited(eff: Effect): Boolean =
      visited.contains(eff)

    def withOwner(sym: Symbol): State = copy(env = env.withOwner(sym))

    def test(op: State ?=> Errors): Errors = {
      val saved = visited
      val errors = op(using this)
      visited = saved
      errors
    }
  }

  given theEnv(using State) as Env = summon[State].env
  given theCtx(using State) as Context = summon[State].env.ctx

  /** Check that the given concrete class may be initialized safely
   *
   *  It assumes that all definitions are properly summarized before-hand.
   *  However, summarization can be done lazily on-demand to improve
   *  performance.
   */
  def checkClassBody(cdef: TypeDef)(implicit state: State): Unit = {
    traceIndented("\n\n>>>> checking " + cdef.symbol.show, init)

    val cls = cdef.symbol.asClass
    val tpl = cdef.rhs.asInstanceOf[Template]

    // mark current class as initialized, required for linearization
    state.parentsInited += cls

    def checkClassBodyStat(tree: Tree)(implicit state: State): Unit = traceOp("checking " + tree.show, init) {
      tree match {
        case vdef : ValDef =>
          val summary = Summarization.analyze(vdef.rhs)
          theEnv.summaryOf(cls).cacheFor(vdef.symbol, summary)
          if (!vdef.symbol.is(Flags.Lazy)) {
            checkEffects(summary.effs)
            traceIndented(vdef.symbol.show + " initialized", init)
            state.fieldsInited += vdef.symbol
          }

        case tree =>
          val summary = Summarization.analyze(tree)
          checkEffects(summary.effs)
      }
    }

    // check parent calls : follows linearization ordering
    // see spec 5.1 about "Template Evaluation".
    // https://www.scala-lang.org/files/archive/spec/2.13/05-classes-and-objects.html

    def checkConstructor(ctor: Symbol, tp: Type, source: Tree)(implicit state: State): Unit = traceOp("checking " + ctor.show, init) {
      val cls = ctor.owner
      val classDef = cls.defTree
      if (!classDef.isEmpty) {
        if (ctor.isPrimaryConstructor) checkClassBody(classDef.asInstanceOf[TypeDef])(state.withOwner(cls))
        else checkSecondaryConstructor(ctor)(state.withOwner(cls))
      }
    }

    def checkSecondaryConstructor(ctor: Symbol)(implicit state: State): Unit = traceOp("checking " + ctor.show, init) {
      val Block(ctorCall :: stats, expr) = ctor.defTree.asInstanceOf[DefDef].rhs
      val cls = ctor.owner.asClass

      traceOp("check ctor: " + ctorCall.show, init) {
        val ctor = ctorCall.symbol
        if (ctor.isPrimaryConstructor)
          checkClassBody(cls.defTree.asInstanceOf[TypeDef])
        else
          checkSecondaryConstructor(ctor)
      }

      (stats :+ expr).foreach { stat =>
        val summary = Summarization.analyze(stat)(theEnv.withOwner(ctor))
        checkEffects(summary.effs)
      }
    }

    cls.paramAccessors.foreach { acc =>
      if (!acc.is(Flags.Method)) {
        traceIndented(acc.show + " initialized", init)
        state.fieldsInited += acc
      }
    }

    tpl.parents.foreach {
      case tree @ Block(_, parent) =>
        val (ctor, _, _) = decomposeCall(parent)
        checkConstructor(ctor.symbol, parent.tpe, tree)

      case tree @ Apply(Block(_, parent), _) =>
        val (ctor, _, _) = decomposeCall(parent)
        checkConstructor(ctor.symbol, tree.tpe, tree)

      case parent : Apply =>
        val (ctor, _, argss) = decomposeCall(parent)
        checkConstructor(ctor.symbol, parent.tpe, parent)

      case ref =>
        val cls = ref.tpe.classSymbol.asClass
        if (!state.parentsInited.contains(cls) && cls.primaryConstructor.exists)
          checkConstructor(cls.primaryConstructor, ref.tpe, ref)
    }

    if cls == state.thisClass then
      state.superCallFinished = true

    // check class body
    tpl.body.foreach { checkClassBodyStat(_) }
  }

  private def checkEffects(effs: Effects)(implicit state: State): Unit = traceOp("checking effects " + Effects.show(effs), init) {
    for {
      eff <- effs
      error <- check(eff)
    } error.issue
  }

  private def checkPromotion(pot: Refinable, source: Tree)(implicit state: State): Errors =
    val buffer = new mutable.ArrayBuffer[Effect]
    val classRef =
      pot match
      case lhot: LocalHot => lhot.classSymbol.typeRef
      case obj: Global => obj.tmref
      case warm: Warm => warm.classSymbol.typeRef
      case _: ThisRef => ??? // impossible

    val accessibleFlags = Flags.Deferred | Flags.Private | Flags.Protected

    classRef.fields.foreach { denot =>
      val f = denot.symbol
      if f.isOneOf(accessibleFlags) then
        buffer += Promote(FieldReturn(pot, f)(source))(source)
        buffer += FieldAccess(pot, f)(source)
    }

    classRef.membersBasedOnFlags(Flags.Method, accessibleFlags).foreach { denot =>
      val m = denot.symbol
      if !theEnv.canIgnoreMethod(m) then
        buffer += MethodCall(pot, m)(source)
        buffer += Promote(MethodReturn(pot, m)(source))(source)
    }

    classRef.memberClasses.foreach { denot =>
      val cls = denot.symbol.asClass
      val potInner =
        pot match
        case warm: Warm => Warm(cls, pot)(source)
        case _ => LocalHot(cls)(source)
      buffer += MethodCall(potInner, cls.primaryConstructor)(source)
      buffer += Promote(potInner)(source)
    }

    buffer.toList.flatMap(eff => check(eff))

  private def check(eff: Effect)(implicit state: State): Errors =
    if (state.hasVisited(eff)) Errors.empty
    else trace("checking effect " + eff.show, init, errs => Errors.show(errs.asInstanceOf[Errors])) {
      implicit val state2: State = state.withVisited(eff)

      eff match {
        case Promote(pot) =>
          pot match {
            case pot: ThisRef =>
              val classRef = state.thisClass.typeRef
              val sum = classRef.fields.foldLeft(0) { (sum, denot) =>
                if denot.symbol.is(Flags.Lazy) then sum
                else sum + 1
              }
              if sum < state.fieldsInited.size then
                PromoteThis(pot, eff.source, state2.path).toErrors
              else
                state.safePromoted += pot
                Errors.empty

            case _: Cold =>
              PromoteCold(eff.source, state2.path).toErrors

            case pot @ Warm(cls, outer) =>
              val errors = state.test { check(Promote(outer)(eff.source)) }
              if (errors.isEmpty) Errors.empty
              else
                val errs = state.test { checkPromotion(pot, eff.source) }
                if errs.nonEmpty then UnsafePromotion(pot, eff.source, state2.path, errs).toErrors
                else
                  state.safePromoted += pot
                  Errors.empty

            case Fun(pots, effs) =>
              val errs1 = state.test {
                effs.toList.flatMap { check(_) }
              }

              val errs2 = state.test {
                pots.toList.flatMap { pot =>
                  check(Promote(pot)(eff.source))(state.copy(path = Vector.empty))
                }
              }

              if (errs1.nonEmpty || errs2.nonEmpty)
                UnsafePromotion(pot, eff.source, state2.path, errs1 ++ errs2).toErrors
              else
                Errors.empty

            case obj @ Global(tmref) =>
              if !state.isGlobalObject then Errors.empty
              else
                val errs = state.test { checkPromotion(obj, eff.source) }
                if errs.nonEmpty then
                  UnsafePromotion(obj, eff.source, state2.path, errs).toErrors
                else
                  state.safePromoted += pot
                  Errors.empty

            case lhot @ LocalHot(cls) =>
              if !state.isGlobalObject then Errors.empty
              else
                val errs = state.test { checkPromotion(lhot, eff.source) }
                if errs.nonEmpty then
                  UnsafePromotion(lhot, eff.source, state2.path, errs).toErrors
                else
                  state.safePromoted += pot
                  Errors.empty

            case pot =>
              val Summary(pots, effs) = expand(pot)
              val effs2 = pots.map(Promote(_)(eff.source))
              (effs2 ++ effs).toList.flatMap(check(_))
          }

        case FieldAccess(pot, field) =>

          pot match {
            case _: ThisRef =>
              val target = resolve(state.thisClass, field)
              if (target.is(Flags.Lazy)) check(MethodCall(pot, target)(eff.source))
              else if (!state.fieldsInited.contains(target)) AccessNonInit(target, state2.path).toErrors
              else Errors.empty

            case SuperRef(_: ThisRef, supercls) =>
              val target = resolveSuper(state.thisClass, supercls, field)
              if (target.is(Flags.Lazy)) check(MethodCall(pot, target)(eff.source))
              else if (!state.fieldsInited.contains(target)) AccessNonInit(target, state2.path).toErrors
              else Errors.empty

            case Warm(cls, outer) =>
              // all fields of warm values are initialized
              val target = resolve(cls, field)
              if (target.is(Flags.Lazy)) check(MethodCall(pot, target)(eff.source))
              else Errors.empty

            case obj @ Global(tmref) =>
              if !state.isGlobalObject || obj.moduleClass != state.thisClass then
                Errors.empty
              else
                val target = resolve(state.thisClass, field)
                if (target.is(Flags.Lazy)) check(MethodCall(pot, target)(eff.source))
                else if (!state.fieldsInited.contains(target)) AccessNonInit(target, state2.path).toErrors
                else Errors.empty

            case _: LocalHot =>
              Errors.empty

            case _: Cold =>
              AccessCold(field, eff.source, state2.path).toErrors

            case Fun(pots, effs) =>
              throw new Exception("Unexpected effect " + eff.show)

            case pot =>
              val Summary(pots, effs) = expand(pot)
              val effs2 = pots.map(FieldAccess(_, field)(eff.source))
              (effs2 ++ effs).toList.flatMap(check(_))

          }

        case MethodCall(pot, sym) =>
          pot match {
            case thisRef: ThisRef =>
              val target = resolve(state.thisClass, sym)
              if (!target.isOneOf(Flags.Method | Flags.Lazy))
                check(FieldAccess(pot, target)(eff.source))
              else if (target.hasSource) {
                val effs = thisRef.effectsOf(target).toList
                effs.flatMap { check(_) }
              }
              else CallUnknown(target, eff.source, state2.path).toErrors

            case SuperRef(thisRef: ThisRef, supercls) =>
              val target = resolveSuper(state.thisClass, supercls, sym)
              if (!target.is(Flags.Method))
                check(FieldAccess(pot, target)(eff.source))
              else if (target.hasSource) {
                val effs = thisRef.effectsOf(target).toList
                effs.flatMap { check(_) }
              }
              else CallUnknown(target, eff.source, state2.path).toErrors

            case warm @ Warm(cls, outer) =>
              val target = resolve(cls, sym)

              if (target.hasSource) {
                val effs = warm.effectsOf(target).toList
                effs.flatMap { check(_) }
              }
              else if (!sym.isConstructor)
                CallUnknown(target, eff.source, state2.path).toErrors
              else
                Errors.empty

            case pot @ Global(tmref) =>
              if !state.isGlobalObject then
                Errors.empty
              else
                val target = resolve(pot.moduleClass, sym)
                if (!target.is(Flags.Method))
                  check(FieldAccess(pot, target)(eff.source))
                else if (target.hasSource) {
                  val effs = pot.effectsOf(target).toList
                  effs.flatMap { check(_) }
                }
                else CallUnknown(target, eff.source, state2.path).toErrors

            case lhot: LocalHot =>
              if !state.isGlobalObject then
                Errors.empty
              else
                val target = resolve(lhot.classSymbol, sym)
                if (!target.is(Flags.Method))
                  check(FieldAccess(pot, target)(eff.source))
                else if (target.hasSource) {
                  val effs = lhot.effectsOf(target).toList
                  effs.flatMap { check(_) }
                }
                else CallUnknown(target, eff.source, state2.path).toErrors

            case _: Cold =>
              CallCold(sym, eff.source, state2.path).toErrors

            case Fun(pots, effs) =>
              // TODO: assertion might be false, due to SAM
              if (sym.name.toString == "apply") effs.toList.flatMap { check(_) }
              else Errors.empty
              // curried, tupled, toString are harmless

            case pot =>
              val Summary(pots, effs) = expand(pot)
              val effs2 = pots.map(MethodCall(_, sym)(eff.source))
              (effs2 ++ effs).toList.flatMap(check(_))
          }

        case AccessGlobal(pot) =>
          if !state.isGlobalObject || pot.moduleClass != state.thisClass then
            Errors.empty
          else if state.superCallFinished then
            Errors.empty
          else
            AccessNonInit(pot.tmref.symbol, state2.path).toErrors

      }
    }

  private def expand(pot: Potential)(implicit state: State): Summary = trace("expand " + pot.show, init, _.asInstanceOf[Summary].show) {
    pot match {
      case MethodReturn(pot1, sym) =>
        pot1 match {
          case thisRef: ThisRef =>
            val target = resolve(state.thisClass, sym)
            if (target.hasSource) Summary(thisRef.potentialsOf(target), Effects.empty)
            else Summary.empty // warning already issued in call effect

          case SuperRef(thisRef: ThisRef, supercls) =>
            val target = resolveSuper(state.thisClass, supercls, sym)
            if (target.hasSource) Summary(thisRef.potentialsOf(target), Effects.empty)
            else Summary.empty // warning already issued in call effect


          case Fun(pots, effs) =>
            val name = sym.name.toString
            if (name == "apply") Summary(pots, Effects.empty)
            else if (name == "tupled") Summary(Set(pot1), Effects.empty)
            else if (name == "curried") {
              val arity = defn.functionArity(sym.info.finalResultType)
              val pots = (1 until arity).foldLeft(Set(pot1)) { (acc, i) =>
                Set(Fun(acc, Effects.empty)(pot1.source))
              }
              Summary(pots, Effects.empty)
            }
            else Summary.empty

          case warm : Warm =>
            val target = resolve(warm.classSymbol, sym)
            if (target.hasSource) Summary(warm.potentialsOf(target), Effects.empty)
            else Summary.empty // warning already issued in call effect

          case lhot: LocalHot =>
            val target = resolve(lhot.classSymbol, sym)
            if (target.hasSource) Summary(lhot.potentialsOf(target), Effects.empty)
            else Summary.empty

          case obj @ Global(tmref) =>
            val target = resolve(obj.moduleClass, sym)
            if (target.hasSource) Summary(obj.potentialsOf(target), Effects.empty)
            else Summary.empty

          case _: Cold =>
            Summary.empty // error already reported, ignore

          case _ =>
            val Summary(pots, effs) = expand(pot1)
            val Summary(pots2, effs2) = pots.select(sym, pot.source, selectEffect = false)
            Summary(pots2, effs ++ effs2)
        }

      case FieldReturn(pot1, sym) =>
        pot1 match {
          case thisRef: ThisRef =>
            val target = resolve(state.thisClass, sym)
            if (sym.hasSource) Summary(thisRef.potentialsOf(target), Effects.empty)
            else Summary(Cold()(pot.source))

          case SuperRef(thisRef: ThisRef, supercls) =>
            val target = resolveSuper(state.thisClass, supercls, sym)
            if (target.hasSource) Summary(thisRef.potentialsOf(target), Effects.empty)
            else Summary(Cold()(pot.source))

          case _: Fun =>
            throw new Exception("Unexpected code reached")

          case warm: Warm =>
            val target = resolve(warm.classSymbol, sym)
            if (target.hasSource) Summary(warm.potentialsOf(target), Effects.empty)
            else Summary(Cold()(pot.source))

          case lhot: LocalHot =>
            val target = resolve(lhot.classSymbol, sym)
            if (target.hasSource) Summary(lhot.potentialsOf(target), Effects.empty)
            else Summary(Cold()(pot.source))

          case obj @ Global(tmref) =>
            val target = resolve(obj.moduleClass, sym)
            if (target.hasSource) Summary(obj.potentialsOf(target), Effects.empty)
            else Summary(Cold()(pot.source))

          case _: Cold =>
            Summary.empty // error already reported, ignore

          case _ =>
            val Summary(pots, effs) = expand(pot1)
            val Summary(pots2, effs2) = pots.select(sym, pot.source, selectEffect = false)
            Summary(pots2, effs ++ effs2)
        }

      case Outer(pot1, cls) =>
        pot1 match {
          case _: ThisRef =>
            // all outers for `this` are assumed to be hot
            Summary.empty

          case _: Fun =>
            throw new Exception("Unexpected code reached")

          case warm: Warm =>
            Summary(warm.resolveOuter(cls))

          case _ =>
            val Summary(pots, effs) = expand(pot1)
            val pots2 = pots.map { Outer(_, cls)(pot.source): Potential }
            Summary(pots2, effs)
        }

      case _: ThisRef | _: Fun | _: Warm | _: Cold | _: Global | _: LocalHot =>
        Summary(pot)

      case SuperRef(pot1, supercls) =>
        val Summary(pots, effs) = expand(pot1)
        val pots2 = pots.map { SuperRef(_, supercls)(pot.source): Potential }
        Summary(pots2, effs)
    }
  }
}
