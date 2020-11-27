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
    var visited: Set[Effect],                  // effects that have been checked or are being checked
    path: Vector[Tree],                        // the path that leads to the current effect
    thisClass: ClassSymbol,                    // the concrete class of `this`
    fieldsInited: mutable.Set[Symbol],
    parentsInited: mutable.Set[ClassSymbol],
    safePromoted: mutable.Set[Potential],      // Potentials that can be safely promoted
    env: Env
  ) {
    def withOwner(sym: Symbol): State = copy(env = env.withOwner(sym))

    def test(op: State ?=> Errors): Errors = {
      val savedVisited = visited
      val errors = op(using this)
      visited = savedVisited
      errors
    }
  }

  given theEnv(using State): Env = summon[State].env
  given theCtx(using State): Context = summon[State].env.ctx

  private def check(eff: Effect)(using state: State): Errors = {
    trace("checking effect " + eff.show, init, errs => Errors.show(errs.asInstanceOf[Errors])) {
      if (state.visited.contains(eff)) {
        traceIndented("Already checked " + eff.show, init)
        Errors.empty
      }
      else {
        state.visited = state.visited + eff
        val state2: State = state.copy(path = state.path :+ eff.source)
        eff match {
          case eff: Promote      => Checking.checkPromote(eff)(using state2)
          case eff: FieldAccess  => Checking.checkFieldAccess(eff)(using state2)
          case eff: MethodCall   => Checking.checkMethodCall(eff)(using state2)
        }
      }
    }
  }

  private def checkEffects(effs: Effects)(using state: State): Unit = traceOp("checking effects " + Effects.show(effs), init) {
    for {
      eff <- effs
      error <- check(eff)
    } error.issue
  }

  /** Check that the given concrete class may be initialized safely
   *
   *  It assumes that all definitions are properly summarized before-hand.
   *  However, summarization can be done lazily on-demand to improve
   *  performance.
   */
  def checkClassBody(cdef: TypeDef)(using state: State): Unit = {
    traceIndented("\n\n>>>> checking " + cdef.symbol.show, init)

    val cls = cdef.symbol.asClass
    val tpl = cdef.rhs.asInstanceOf[Template]

    // mark current class as initialized, required for linearization
    state.parentsInited += cls

    def checkClassBodyStat(tree: Tree)(using state: State): Unit = traceOp("checking " + tree.show, init) {
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

    def checkConstructor(ctor: Symbol, tp: Type, source: Tree)(using state: State): Unit = traceOp("checking " + ctor.show, init) {
      val cls = ctor.owner
      val classDef = cls.defTree
      if (!classDef.isEmpty) {
        given State = state.withOwner(cls)
        if (ctor.isPrimaryConstructor) checkClassBody(classDef.asInstanceOf[TypeDef])
        else checkSecondaryConstructor(ctor)
      }
    }

    def checkSecondaryConstructor(ctor: Symbol)(using state: State): Unit = traceOp("checking " + ctor.show, init) {
      val Block(ctorCall :: stats, expr) = ctor.defTree.asInstanceOf[DefDef].rhs
      val cls = ctor.owner.asClass

      traceOp("check ctor: " + ctorCall.show, init) {
        val ctor = ctorCall.symbol
        if (ctor.isPrimaryConstructor)
          checkClassBody(cls.defTree.asInstanceOf[TypeDef])
        else
          checkSecondaryConstructor(ctor)
      }

      checkStats(stats :+ expr, ctor)
    }

    def checkStats(stats: List[Tree], owner: Symbol)(using state: State): Unit =
      stats.foreach { stat =>
        val summary = Summarization.analyze(stat)(theEnv.withOwner(owner))
        checkEffects(summary.effs)
      }

    cls.paramAccessors.foreach { acc =>
      if (!acc.is(Flags.Method)) {
        traceIndented(acc.show + " initialized", init)
        state.fieldsInited += acc
      }
    }

    tpl.parents.foreach {
      case tree @ Block(_, parent) =>
        checkConstructor(funPart(parent).symbol, parent.tpe, tree)

      case tree @ Apply(Block(_, parent), _) =>
        checkConstructor(funPart(parent).symbol, tree.tpe, tree)

      case parent : Apply =>
        checkConstructor(funPart(parent).symbol, parent.tpe, parent)

      case ref =>
        val cls = ref.tpe.classSymbol.asClass
        if (!state.parentsInited.contains(cls) && cls.primaryConstructor.exists)
          checkConstructor(cls.primaryConstructor, ref.tpe, ref)
    }

    // check class body
    tpl.body.foreach { checkClassBodyStat(_) }
  }

  private def checkMethodCall(eff: MethodCall)(using state: State): Errors =
    val MethodCall(pot, sym) = eff
    pot match {
      case thisRef: ThisRef =>
        val target = resolve(state.thisClass, sym)
        if (!target.isOneOf(Flags.Method | Flags.Lazy))
          check(FieldAccess(pot, target)(eff.source))
        else if (target.hasSource) {
          val effs = thisRef.effectsOf(target).toList
          effs.flatMap { check(_) }
        }
        else CallUnknown(target, eff.source, state.path).toErrors

      case SuperRef(thisRef: ThisRef, supercls) =>
        val target = resolveSuper(state.thisClass, supercls, sym)
        if (!target.is(Flags.Method))
          check(FieldAccess(pot, target)(eff.source))
        else if (target.hasSource) {
          val effs = thisRef.effectsOf(target).toList
          effs.flatMap { check(_) }
        }
        else CallUnknown(target, eff.source, state.path).toErrors

      case warm @ Warm(cls, outer) =>
        val target = resolve(cls, sym)

        if (target.hasSource) {
          val effs = warm.effectsOf(target).toList
          effs.flatMap { check(_) }
        }
        else if (!sym.isConstructor)
          CallUnknown(target, eff.source, state.path).toErrors
        else
          Errors.empty

      case _: Cold =>
        CallCold(sym, eff.source, state.path).toErrors

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

  private def checkFieldAccess(eff: FieldAccess)(using state: State): Errors =
    val FieldAccess(pot, field) = eff
    pot match {
      case _: ThisRef =>
        val target = resolve(state.thisClass, field)
        if (target.is(Flags.Lazy)) check(MethodCall(pot, target)(eff.source))
        else if (!state.fieldsInited.contains(target)) AccessNonInit(target, state.path).toErrors
        else Errors.empty

      case SuperRef(_: ThisRef, supercls) =>
        val target = resolveSuper(state.thisClass, supercls, field)
        if (target.is(Flags.Lazy)) check(MethodCall(pot, target)(eff.source))
        else if (!state.fieldsInited.contains(target)) AccessNonInit(target, state.path).toErrors
        else Errors.empty

      case Warm(cls, outer) =>
        // all fields of warm values are initialized
        val target = resolve(cls, field)
        if (target.is(Flags.Lazy)) check(MethodCall(pot, target)(eff.source))
        else Errors.empty

      case _: Cold =>
        AccessCold(field, eff.source, state.path).toErrors

      case Fun(pots, effs) =>
        throw new Exception("Unexpected effect " + eff.show)

      case pot =>
        val Summary(pots, effs) = expand(pot)
        val effs2 = pots.map(FieldAccess(_, field)(eff.source))
        (effs2 ++ effs).toList.flatMap(check(_))

    }


  private def checkPromote(eff: Promote)(using state: State): Errors =
    if (state.safePromoted.contains(eff.potential)) Errors.empty
    else {
      val pot = eff.potential
      val errs = pot match {
        case pot: ThisRef =>
          // If we have all fields initialized, then we can promote This to hot.
          val classRef = state.thisClass.info.asInstanceOf[ClassInfo].appliedRef
          val allFieldsInited = classRef.fields.forall { denot =>
            val sym = denot.symbol
            sym.isOneOf(Flags.Lazy | Flags.Deferred) || state.fieldsInited.contains(sym)
          }
          if (allFieldsInited)
            Errors.empty
          else
            PromoteThis(pot, eff.source, state.path).toErrors

        case _: Cold =>
          PromoteCold(eff.source, state.path).toErrors

        case pot @ Warm(cls, outer) =>
          val errors = state.test { checkPromote(Promote(outer)(eff.source)) }
          if (errors.isEmpty) Errors.empty
          else PromoteWarm(pot, eff.source, state.path).toErrors

        case Fun(pots, effs) =>
          val errs1 = state.test {
            effs.toList.flatMap(check(_))
          }
          val errs2 = state.test {
            pots.toList.flatMap { pot =>
              checkPromote(Promote(pot)(eff.source))
            }
          }

          if (errs1.nonEmpty || errs2.nonEmpty)
            UnsafePromotion(pot, eff.source, state.path, errs1 ++ errs2).toErrors
          else
            Errors.empty

        case pot =>
          val Summary(pots, effs) = expand(pot)
          val effs2 = pots.map(Promote(_)(eff.source))
          (effs2 ++ effs).toList.flatMap(check(_))
      }
      // If we can safely promote, then we don't need to check again
      if (errs.isEmpty)
        state.safePromoted += pot
      errs
    }

  private def expand(pot: Potential)(using state: State): Summary = trace("expand " + pot.show, init, _.asInstanceOf[Summary].show) {
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

          case _: Cold =>
            Summary.empty // error already reported, ignore

          case _ =>
            val Summary(pots, effs) = expand(pot1)
            val Summary(pots2, effs2) = pots.select(sym, pot.source)
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

          case _: Cold =>
            Summary.empty // error already reported, ignore

          case _ =>
            val Summary(pots, effs) = expand(pot1)
            val Summary(pots2, effs2) = pots.select(sym, pot.source)
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

      case _: ThisRef | _: Fun | _: Warm | _: Cold =>
        Summary(pot)

      case SuperRef(pot1, supercls) =>
        val Summary(pots, effs) = expand(pot1)
        val pots2 = pots.map { SuperRef(_, supercls)(pot.source): Potential }
        Summary(pots2, effs)
    }
  }
}
