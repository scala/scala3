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

import Effects._, Potentials._, Summary._, Util._

object Checking {
  /** The checking state
   *
   *  Why `visited` is a set of `MethodCall` instead of `Symbol`? Think the following program:
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
    def withVisited(eff: Effect)(implicit ctx: Context): State = {
      visited += eff
      copy(path = this.path :+ eff.source)
    }

    def trace(implicit state: State): String = {
      var indentCount = 0
      var last = ""
      val sb = new StringBuilder
      this.path.foreach { tree =>
        indentCount += 1
        val pos = tree.sourcePos
        val line = "[ " + pos.source.file.toString + ":" + (pos.line + 1) + " ]"
        if (last != line)
          sb.append(
            if (pos.source.exists)
              i"${ " " * indentCount }-> ${pos.lineContent.trim}\t$line\n"
            else
              i"${tree.show}\n"
          )
        last = line
      }
      sb.toString
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
          val (pots, effs) = Summarization.analyze(vdef.rhs)(ctx.withOwner(vdef.symbol))
          theEnv.summaryOf(cls).cacheFor(vdef.symbol, (pots, effs))
          if (!vdef.symbol.is(Flags.Lazy)) {
            traceIndented(vdef.symbol.show + " initialized", init)
            checkEffects(effs)
            state.fieldsInited += vdef.symbol
          }

        case tree =>
          val (_, effs) = Summarization.analyze(tree)
          checkEffects(effs)
      }
    }

    def checkEffects(effs: Effects): Unit = {
      val rebased = Effects.asSeenFrom(effs, ThisRef(state.thisClass)(null), cls, Potentials.empty)
      rebased.foreach { check(_) }
    }

    // check parent calls : follows linearization ordering
    // see spec 5.1 about "Template Evaluation".
    // https://www.scala-lang.org/files/archive/spec/2.13/05-classes-and-objects.html#class-linearization

    def checkStats(stats: List[Tree])(implicit ctx: Context): Unit =
      stats.foreach { stat =>
        val (_, effs) = Summarization.analyze(stat)
        checkEffects(effs)
      }

    def checkCtor(ctor: Symbol, tp: Type, source: Tree)(implicit ctx: Context): Unit = {
      val cls = ctor.owner
      val classDef = cls.defTree
      if (!classDef.isEmpty) {
        // TODO: no need to check, can only be empty
        // val (pots, effs) = tp.typeConstructor match {
        //   case tref: TypeRef => Summarization.analyze(tref.prefix, source)
        // }
        // checkEffects(effs)

        if (ctor.isPrimaryConstructor) checkClassBody(classDef.asInstanceOf[TypeDef])
        else checkSecondaryConstructor(ctor)
      }
      else if (!cls.isOneOf(Flags.EffectivelyOpenFlags))
        ctx.warning("Inheriting non-open class may cause initialization errors", source.sourcePos)
    }

    tpl.parents.foreach {
      case tree @ Block(stats, parent) =>
        val (ctor, _, argss) = decomposeCall(parent)
        checkStats(stats)
        checkStats(argss.flatten)
        checkCtor(ctor.symbol, parent.tpe, tree)

      case tree @ Apply(Block(stats, parent), args) =>
        val (ctor, _, argss) = decomposeCall(parent)
        checkStats(stats)
        checkStats(args)
        checkStats(argss.flatten)
        checkCtor(ctor.symbol, tree.tpe, tree)

      case parent : Apply =>
        val (ctor, _, argss) = decomposeCall(parent)
        checkStats(argss.flatten)
        checkCtor(ctor.symbol, parent.tpe, parent)

      case ref =>
        val cls = ref.symbol.asClass
        if (!state.parentsInited.contains(cls))
          checkCtor(cls.primaryConstructor, ref.tpe, ref)
    }

    // check class body
    tpl.body.foreach { checkClassBodyStat(_) }
  }

  def checkSecondaryConstructor(ctor: Symbol)(implicit state: State): Unit = traceOp("checking " + ctor.show, init) {
    val Block(ctorCall :: stats, expr) = ctor.defTree
    val cls = ctor.owner.asClass

    traceOp("check ctor: " + ctor.show, init) {
      if (ctor.isPrimaryConstructor)
        checkClassBody(cls.defTree.asInstanceOf[TypeDef])
      else
        checkSecondaryConstructor(ctor)
    }

    (stats :+ expr).foreach { stat =>
      val (_, effs) = Summarization.analyze(stat)(theCtx.withOwner(ctor))
      val rebased = Effects.asSeenFrom(effs, ThisRef(state.thisClass)(null), cls, Potentials.empty)
      rebased.foreach { check(_) }
    }
  }

  private def check(eff: Effect)(implicit state: State): Unit =
    if (!state.visited.contains(eff)) traceOp("checking effect " + eff.show, init) {
      eff match {
        case Leak(pot) =>
          pot match {
            case ThisRef(cls) =>
              assert(cls == state.thisClass, "unexpected potential " + pot.show)

              theCtx.warning(
                "Leaking of this. Calling trace:\n" + state.trace,
                eff.source.sourcePos
              )

            case _: Cold =>
              theCtx.warning(
                "Leaking of cold value " + eff.source.show +
                ". Calling trace:\n" + state.trace,
                eff.source.sourcePos
              )

            case Warm(cls, outer) =>
              theCtx.warning(
                "Leaking of warm value " + eff.source.show +
                ". Calling trace:\n" + state.trace,
                eff.source.sourcePos
              )

            case Fun(pots, effs) =>
              val state2 = state.withVisited(eff)
              effs.foreach { check(_)(state2) }
              pots.foreach { pot => check(Leak(pot)(eff.source))(state2) }

            case pot =>
              val state2 = state.withVisited(eff)
              val pots = expand(pot)
              pots.foreach { pot => check(Leak(pot)(eff.source))(state2) }
          }

        case FieldAccess(pot, field) =>
          pot match {
            case ThisRef(cls) =>
              assert(cls == state.thisClass, "unexpected potential " + pot.show)

              if (!state.fieldsInited.contains(field) && !field.is(Flags.Lazy)) {
                traceIndented("initialized: " + state.fieldsInited, init)

                // should issue error, use warning so that it will continue compile subprojects
                theCtx.warning(
                  "Access non-initialized field " + eff.source.show +
                  ". Calling trace:\n" + state.trace,
                  eff.source.sourcePos
                )
              }
              else if (field.is(Flags.Lazy)) {
                // TODO: get effects, rebase and check
              }

            case Warm(cls, outer) =>
              // all fields of warm values are initialized

              if (field.is(Flags.Lazy)) {
                // TODO: get effects, rebase and check
              }

            case _: Cold =>
              theCtx.warning(
                "Access field " + eff.source.show + " on a known value under initialization" +
                ". Calling trace:\n" + state.trace,
                eff.source.sourcePos
              )

            case Fun(pots, effs) =>
              throw new Exception("Unexpected effect " + eff.show)

            case pot =>
              val state2 = state.withVisited(eff)
              val pots = expand(pot)
              pots.foreach { pot => check(FieldAccess(pot, field)(eff.source))(state2) }
          }

        case MethodCall(pot, sym, virtual) =>
          pot match {
            case thisRef @ ThisRef(cls) =>
              assert(cls == state.thisClass, "unexpected potential " + pot.show)

              if (sym.isInternal) { // tests/init/override17.scala
                val cls = sym.owner.asClass
                val effs = theEnv.summaryOf(cls).effectsOf(sym)
                val rebased = Effects.asSeenFrom(effs, thisRef, cls, Potentials.empty)
                val state2 = state.withVisited(eff)
                rebased.foreach { check(_)(state2) }
              }
              else
                theCtx.warning(
                  "Calling the external method " + sym.show +
                  " may cause initialization errors" +
                  ". Calling trace:\n" + state.trace,
                  eff.source.sourcePos
                )

            case warm @ Warm(cls, outer) =>
              if (sym.isInternal) {
                val cls = sym.owner.asClass
                val effs = theEnv.summaryOf(cls).effectsOf(sym)
                val outer = Potentials.empty + Outer(warm, cls)(warm.source)
                val rebased = Effects.asSeenFrom(effs, warm, cls, outer)
                val state2 = state.withVisited(eff)
                rebased.foreach { check(_)(state2) }
              }
              else
                theCtx.warning(
                  "Calling the external method " + sym.show +
                  " on uninitialized objects may cause initialization errors" +
                  ". Calling trace:\n" + state.trace,
                  eff.source.sourcePos
                )

            case _: Cold =>
              theCtx.warning(
                "Call method " + eff.source.show + " on a cold value" +
                ". Calling trace:\n" + state.trace,
                eff.source.sourcePos
              )

            case Fun(pots, effs) =>
              // TODO: assertion might be false, due to SAM
              if (sym.name.toString == "apply") {
                val state2 = state.withVisited(eff)
                effs.foreach { check(_)(state2) }
                pots.foreach { pot => check(Leak(pot)(eff.source))(state2) }
              }
              // curried, tupled, toString are harmless

            case pot =>
              val state2 = state.withVisited(eff)
              val pots = expand(pot)
              pots.foreach { pot =>
                check(MethodCall(pot, sym, virtual)(eff.source))(state2)
              }
          }
      }
    }

  private def expand(pot: Potential)(implicit state: State): Potentials =
    trace("expand " + pot.show, init, pots => Potentials.show(pots.asInstanceOf[Potentials])) { pot match {
      case MethodReturn(pot1, sym, virtual) =>
        pot1 match {
          case thisRef @ ThisRef(cls) =>
            assert(cls == state.thisClass, "unexpected potential " + pot.show)

            if (sym.isInternal) { // tests/init/override17.scala
              val cls = sym.owner.asClass
              val pots = theEnv.summaryOf(cls).potentialsOf(sym)
              Potentials.asSeenFrom(pots, thisRef, cls, Potentials.empty)
            }
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
            if (sym.isInternal) {
              val cls = sym.owner.asClass
              val pots = theEnv.summaryOf(cls).potentialsOf(sym)
              val outer = Potentials.empty + Outer(warm, cls)(warm.source)
              Potentials.asSeenFrom(pots, warm, cls, outer)
            }
            else Potentials.empty // warning already issued in call effect

          case _: Cold =>
            Potentials.empty // error already reported, ignore

          case _ =>
            val (pots, effs) = expand(pot1).select(sym, pot.source, virtual)
            effs.foreach(check(_))
            pots
        }

      case FieldReturn(pot1, sym) =>
        pot1 match {
          case thisRef @ ThisRef(cls) =>
            assert(cls == state.thisClass, "unexpected potential " + pot.show)

            if (sym.isInternal) { // tests/init/override17.scala
              val cls = sym.owner.asClass
              val pots = theEnv.summaryOf(cls).potentialsOf(sym)
              Potentials.asSeenFrom(pots, thisRef, cls, Potentials.empty)
            }
            else Potentials.empty + Cold()(pot.source)

          case _: Fun =>
            throw new Exception("Unexpected code reached")

          case warm: Warm =>
            if (sym.isInternal) {
              val cls = sym.owner.asClass
              val pots = theEnv.summaryOf(cls).potentialsOf(sym)
              val outer = Potentials.empty + (Outer(warm, cls)(warm.source))
              Potentials.asSeenFrom(pots, warm, cls, outer)
            }
            else Potentials.empty + Cold()(pot.source)

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

      case _: SuperRef =>
        throw new Exception("Unexpected SuperRef")
    }
  }
}
