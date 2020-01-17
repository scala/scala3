package dotty.tools.dotc
package transform
package init

import scala.collection.mutable

import core._
import Contexts.Context
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
    visited: mutable.Set[Effect],              // effects that have been expanded
    path: Vector[Tree],                        // the path that leads to the current effect
    thisClass: ClassSymbol,                    // the concrete class of `this`
    fieldsInited: mutable.Set[Symbol],
    parentsInited: mutable.Set[ClassSymbol],
    env: Env
  ) {
    def withVisited(eff: Effect): State = {
      visited += eff
      copy(path = this.path :+ eff.source)
    }
  }

  private implicit def theEnv(implicit state: State): Env = state.env

  /** Check that the given concrete class may be initialized safely
   *
   *  It assumes that all definitions are properly summarized before-hand.
   *  However, summarization can be done lazily on-demand to improve
   *  performance.
   */
  def checkClassBody(cdef: TypeDef)(implicit state: State): Unit = traceOp("checking " + cdef.symbol.show, init) {
    val cls = cdef.symbol.asClass
    val tpl = cdef.rhs.asInstanceOf[Template]

    // mark current class as initialized, required for linearization
    state.parentsInited += cls

    def checkClassBodyStat(tree: Tree)(implicit ctx: Context): Unit = traceOp("checking " + tree.show, init) {
      tree match {
        case vdef : ValDef =>
          val (pots, effs) = Summarization.analyze(vdef.rhs)(theEnv.withOwner(vdef.symbol))
          theEnv.summaryOf(cls).cacheFor(vdef.symbol, (pots, effs))
          if (!vdef.symbol.is(Flags.Lazy)) {
            checkEffectsIn(effs, cls)
            traceIndented(vdef.symbol.show + " initialized", init)
            state.fieldsInited += vdef.symbol
          }

        case tree =>
          val (_, effs) = Summarization.analyze(tree)
          checkEffectsIn(effs, cls)
      }
    }

    // check parent calls : follows linearization ordering
    // see spec 5.1 about "Template Evaluation".
    // https://www.scala-lang.org/files/archive/spec/2.13/05-classes-and-objects.html

    def checkCtor(ctor: Symbol, tp: Type, source: Tree)(implicit ctx: Context): Unit = {
      val cls = ctor.owner
      val classDef = cls.defTree
      if (!classDef.isEmpty) {
        if (ctor.isPrimaryConstructor) checkClassBody(classDef.asInstanceOf[TypeDef])
        else checkSecondaryConstructor(ctor)
      }
      else if (!cls.isOneOf(Flags.EffectivelyOpenFlags))
        ctx.warning("Inheriting non-open class may cause initialization errors", source.sourcePos)
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
        checkCtor(ctor.symbol, parent.tpe, tree)

      case tree @ Apply(Block(_, parent), _) =>
        val (ctor, _, _) = decomposeCall(parent)
        checkCtor(ctor.symbol, tree.tpe, tree)

      case parent : Apply =>
        val (ctor, _, argss) = decomposeCall(parent)
        checkCtor(ctor.symbol, parent.tpe, parent)

      case ref =>
        val cls = ref.tpe.classSymbol.asClass
        if (!state.parentsInited.contains(cls))
          checkCtor(cls.primaryConstructor, ref.tpe, ref)
    }

    // check class body
    tpl.body.foreach { checkClassBodyStat(_) }
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
      val (_, effs) = Summarization.analyze(stat)(theEnv.withOwner(ctor))
      checkEffectsIn(effs, cls)
    }
  }

  private def checkEffectsIn(effs: Effects, cls: ClassSymbol)(implicit state: State): Unit = {
    val rebased = Effects.asSeenFrom(effs, ThisRef(state.thisClass)(null), cls, Potentials.empty)
    for {
      eff <- rebased
      error <- check(eff)
    } error.report
  }

  private def check(eff: Effect)(implicit state: State): Errors =
    if (state.visited.contains(eff)) Errors.empty else trace("checking effect " + eff.show, init, errs => Errors.show(errs.asInstanceOf[Errors])) {
      implicit val state2: State = state.withVisited(eff)

      eff match {
        case Promote(pot) =>
          pot match {
            case pot @ ThisRef(cls) =>
              assert(cls == state.thisClass, "unexpected potential " + pot.show)
              PromoteThis(pot, eff.source, state2.path).toErrors

            case _: Cold =>
              PromoteCold(eff.source, state2.path).toErrors

            case pot @ Warm(cls, outer) =>
              PromoteWarm(pot, eff.source, state2.path).toErrors

            case Fun(pots, effs) =>
              val errs1 = effs.flatMap { check(_) }
              val errs2 = pots.flatMap { pot => check(Promote(pot)(eff.source))(state.copy(path = Vector.empty)) }
              if (errs1.nonEmpty || errs2.nonEmpty)
                UnsafePromotion(pot, eff.source, state2.path, errs1 ++ errs2).toErrors
              else
                Errors.empty

            case pot =>
              val pots = expand(pot)
              pots.flatMap { pot => check(Promote(pot)(eff.source)) }
          }

        case FieldAccess(pot, field) =>

          pot match {
            case ThisRef(cls) =>
              assert(cls == state.thisClass, "unexpected potential " + pot.show)

              val target = resolve(cls, field)
              if (target.is(Flags.Lazy)) check(MethodCall(pot, target)(eff.source))
              else if (!state.fieldsInited.contains(target)) AccessNonInit(target, state2.path).toErrors
              else Errors.empty

            case SuperRef(ThisRef(cls), supercls) =>
              assert(cls == state.thisClass, "unexpected potential " + pot.show)

              val target = resolveSuper(cls, supercls, field)
              if (target.is(Flags.Lazy)) check(MethodCall(pot, target)(eff.source))
              else if (!state.fieldsInited.contains(target)) AccessNonInit(target, state2.path).toErrors
              else Errors.empty

            case Warm(cls, outer) =>
              // all fields of warm values are initialized
              val target = resolve(cls, field)
              if (target.is(Flags.Lazy)) check(MethodCall(pot, target)(eff.source))
              else Errors.empty

            case _: Cold =>
              AccessCold(field, eff.source, state2.path).toErrors

            case Fun(pots, effs) =>
              throw new Exception("Unexpected effect " + eff.show)

            case pot =>
              val pots = expand(pot)
              pots.flatMap { pot => check(FieldAccess(pot, field)(eff.source)) }
          }

        case MethodCall(pot, sym) =>
          pot match {
            case thisRef @ ThisRef(cls) =>
              assert(cls == state.thisClass, "unexpected potential " + pot.show)

              val target = resolve(cls, sym)
              if (!target.isOneOf(Flags.Method | Flags.Lazy))
                check(FieldAccess(pot, target)(eff.source))
              else if (target.isInternal) {
                val effs = thisRef.effectsOf(target)
                effs.flatMap { check(_) }
              }
              else CallUnknown(target, eff.source, state2.path).toErrors

            case SuperRef(thisRef @ ThisRef(cls), supercls) =>
              assert(cls == state.thisClass, "unexpected potential " + pot.show)

              val target = resolveSuper(cls, supercls, sym)
              if (!target.is(Flags.Method))
                check(FieldAccess(pot, target)(eff.source))
              else if (target.isInternal) {
                val effs = thisRef.effectsOf(target)
                effs.flatMap { check(_) }
              }
              else CallUnknown(target, eff.source, state2.path).toErrors

            case warm @ Warm(cls, outer) =>
              val target = resolve(cls, sym)

              if (target.isInternal) {
                val effs = warm.effectsOf(target)
                effs.flatMap { check(_) }
              }
              else if (!sym.isConstructor) CallUnknown(target, eff.source, state2.path).toErrors
              else Errors.empty

            case _: Cold =>
              CallCold(sym, eff.source, state2.path).toErrors

            case Fun(pots, effs) =>
              // TODO: assertion might be false, due to SAM
              if (sym.name.toString == "apply") effs.flatMap { check(_) }
              else Errors.empty
              // curried, tupled, toString are harmless

            case pot =>
              val pots = expand(pot)
              pots.flatMap { pot =>
                check(MethodCall(pot, sym)(eff.source))
              }
          }
      }
    }

  private def expand(pot: Potential)(implicit state: State): Potentials = trace("expand " + pot.show, init, pots => Potentials.show(pots.asInstanceOf[Potentials])) {
    pot match {
      case MethodReturn(pot1, sym) =>
        pot1 match {
          case thisRef @ ThisRef(cls) =>
            assert(cls == state.thisClass, "unexpected potential " + pot.show)

            val target = resolve(cls, sym)
            if (target.isInternal) thisRef.potentialsOf(target)
            else Potentials.empty // warning already issued in call effect

          case SuperRef(thisRef @ ThisRef(cls), supercls) =>
            assert(cls == state.thisClass, "unexpected potential " + pot.show)

            val target = resolveSuper(cls, supercls, sym)
            if (target.isInternal) thisRef.potentialsOf(target)
            else Potentials.empty // warning already issued in call effect


          case Fun(pots, effs) =>
            val name = sym.name.toString
            if (name == "apply") pots
            else if (name == "tupled") Set(pot1)
            else if (name == "curried") {
              val arity = defn.functionArity(sym.info.finalResultType)
              (1 until arity).foldLeft(Set(pot1)) { (acc, i) => Set(Fun(acc, Effects.empty)(pot1.source)) }
            }
            else Potentials.empty

          case warm : Warm =>
            val target = resolve(warm.classSymbol, sym)
            if (target.isInternal) warm.potentialsOf(target)
            else Potentials.empty // warning already issued in call effect

          case _: Cold =>
            Potentials.empty // error already reported, ignore

          case _ =>
            val (pots, effs) = expand(pot1).select(sym, pot.source)
            effs.foreach(check(_))
            pots
        }

      case FieldReturn(pot1, sym) =>
        pot1 match {
          case thisRef @ ThisRef(cls) =>
            assert(cls == state.thisClass, "unexpected potential " + pot.show)

            val target = resolve(cls, sym)
            if (sym.isInternal) thisRef.potentialsOf(target)
            else Cold()(pot.source).toPots

          case SuperRef(thisRef @ ThisRef(cls), supercls) =>
            assert(cls == state.thisClass, "unexpected potential " + pot.show)

            val target = resolveSuper(cls, supercls, sym)
            if (target.isInternal) thisRef.potentialsOf(target)
            else Cold()(pot.source).toPots

          case _: Fun =>
            throw new Exception("Unexpected code reached")

          case warm: Warm =>
            val target = resolve(warm.classSymbol, sym)
            if (target.isInternal) warm.potentialsOf(target)
            else Cold()(pot.source).toPots

          case _: Cold =>
            Potentials.empty // error already reported, ignore

          case _ =>
            val (pots, effs) = expand(pot1).select(sym, pot.source)
            effs.foreach(check(_))
            pots
        }

      case Outer(pot1, cls) =>
        pot1 match {
          case ThisRef(cls) =>
            assert(cls == state.thisClass, "unexpected potential " + pot.show)

            Potentials.empty

          case _: Fun =>
            throw new Exception("Unexpected code reached")

          case warm: Warm =>
            warm.outerFor(cls)

          case _: Cold =>
            throw new Exception("Unexpected code reached")

          case _ =>
            expand(pot1).map { Outer(_, cls)(pot.source) }
        }

      case _: ThisRef | _: Fun | _: Warm | _: Cold =>
        Set(pot)

      case SuperRef(pot1, supercls) =>
        expand(pot1).map { SuperRef(_, supercls)(pot.source) }
    }
  }
}
