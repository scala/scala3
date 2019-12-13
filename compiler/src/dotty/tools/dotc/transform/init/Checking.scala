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
  def checkClassBody(cdef: TypeDef, outer: Potentials)(implicit state: State): Unit = traceOp("checking " + cdef.symbol.show, init) {
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
            effs.foreach { check(_) }
            state.fieldsInited += vdef.symbol
          }

        case tree =>
          val (_, effs) = Summarization.analyze(tree)
          effs.foreach { check(_)(state) }
      }
    }

    // check parent calls : follows linearization ordering
    // see spec 5.1 about "Template Evaluation".
    // https://www.scala-lang.org/files/archive/spec/2.13/05-classes-and-objects.html#class-linearization

    def checkStats(stats: List[Tree])(implicit ctx: Context): Unit =
      stats.foreach { stat =>
        val (_, effs) = Summarization.analyze(stat)
        effs.foreach { check(_) }
      }

    def checkCtor(ctor: Symbol, tp: Type, source: Tree)(implicit ctx: Context): Unit = {
      val cls = ctor.owner
      val classDef = cls.defTree
      if (!classDef.isEmpty) {
        val (pots, effs) = tp.typeConstructor match {
          case tref: TypeRef => Summarization.analyze(tref.prefix, source)
        }

        // TODO: no need to check, can only be empty
        effs.foreach { check(_) }

        if (ctor.isPrimaryConstructor) checkClassBody(classDef.asInstanceOf[TypeDef], pots)
        else checkSecondaryConstructor(ctor, outer)
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

  def checkSecondaryConstructor(ctor: Symbol, outer: Potentials)(implicit state: State): Unit = traceOp("checking " + ctor.show, init) {
    val Block(ctorCall :: stats, expr) = ctor.defTree
    val ctorCallSym = ctorCall.symbol

    traceOp("check ctor: " + ctor.show, init) {
      if (ctorCallSym.isPrimaryConstructor)
        checkClassBody(ctorCallSym.owner.defTree.asInstanceOf[TypeDef], outer)
      else
        checkSecondaryConstructor(ctorCallSym, outer)
    }

    (stats :+ expr).foreach { stat =>
      val (_, effs) = Summarization.analyze(stat)(theCtx.withOwner(ctor))
      effs.foreach { check(_)(state) }
    }
  }


  /** Resolve possible overriding of the term symbol `sym` relative to `thisClass` */
  private def resolveVirtual(thisClass: ClassSymbol, sym: Symbol)(implicit ctx: Context): Symbol =
    if (sym.isEffectivelyFinal) sym
    else sym.matchingMember(thisClass.typeRef)

  private def check(eff: Effect)(implicit state: State): Unit =
    if (!state.visited.contains(eff)) traceOp("checking effect " + eff.show, init) {
      eff match {
        case Leak(pot) =>
          pot match {
            case ThisRef(tp) =>
              theCtx.warning(
                "Leaking of this. Calling trace:\n" + state.trace,
                eff.source.sourcePos
              )

            case Cold(cls, _) =>
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
              if (
                cls eq state.thisClass &&
                !state.fieldsInited.contains(field) &&
                !field.is(Flags.Lazy)
              ) {
                traceIndented("initialized: " + state.fieldsInited, init)

                // should issue error, use warning so that it will continue compile subprojects
                theCtx.warning(
                  "Access non-initialized field " + eff.source.show +
                  ". Calling trace:\n" + state.trace,
                  eff.source.sourcePos
                )
              }

            case Warm(cls) =>
              // all fields of warm values are initialized

            case Cold(cls, _) =>
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
            case ThisRef(cls) =>
              if (cls eq state.thisClass) {
                // overriding resolution
                val targetSym = if (virtual) resolveVirtual(state.currentClass, sym) else sym
                if (targetSym.exists) { // tests/init/override17.scala
                  val effs = theEnv.effectsOf(targetSym)
                  val state2 = state.withVisited(eff)
                  effs.foreach { check(_)(state2) }
                }
                else {
                  traceIndented("!!!sym does not exist: " + pot.show)
                }
              }

            case Warm(cls) =>
              // overriding resolution
              val targetSym = if (virtual) resolveVirtual(cls, sym) else sym
              val effs = theEnv.effectsOf(targetSym)
              val state2 = state.withVisited(eff)
              effs.foreach { eff =>
                val effs = substitute(eff, Map.empty, Some(cls -> pot))
                effs.foreach { eff2 => check(eff2)(state2) }
              }

            case Dependent(cls, bindings) =>
              // overriding resolution
              val targetSym = if (virtual) resolveVirtual(cls, sym) else sym
              val effs = theEnv.effectsOf(targetSym)
              val state2 = state.withVisited(eff)
              effs.foreach { eff =>
                val effs = substitute(eff, bindings, Some(cls -> pot))
                effs.foreach { eff2 => check(eff2)(state2) }
              }

            case Cold(cls, _) =>
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

        case DependentCall(pot, sym, bindings) =>
          assert(sym.isConstructor, "only dependent call of contructor supported")
          val state2 = state.withVisited(eff)
          pot match {
            // `Cold(cls).init(args)`: encoding of new expressions
            case Cold(cls, _) =>
              val effs = theEnv.effectsOf(sym)
              if (sym.isPrimaryConstructor) {
                val bindings2 = Summarization.dependentParamBindings(sym, bindings)
                val potDep = Dependent(cls, bindings2)(eff.source)
                effs.foreach { eff =>
                  val effs = substitute(eff, bindings, Some(cls -> potDep))
                  effs.foreach { eff2 => check(eff2)(state2) }
                }
              }
              else {
                val effs = theEnv.effectsOf(sym)
                val potDeps = expand(DependentReturn(pot, sym, bindings)(eff.source))
                assert(potDeps.size == 1, "expect size = 1, found " + potDeps.size)
                effs.foreach { eff =>
                  val effs = substitute(eff, bindings, Some(cls -> potDeps.head))
                  effs.foreach { eff2 => check(eff2)(state2) }
                }
              }

            case ThisRef(cls) =>
              assert(
                state.currentClass.derivesFrom(cls),
                state.currentClass.show + " not derived from " + cls.show
              ) // in current class hierachy
              if (sym.exists) { // tests/init/override17.scala
                val effs = theEnv.effectsOf(sym)
                val state2 = state.withVisited(eff)
                effs.foreach { eff =>
                  val effs = substitute(eff, bindings, None)
                  effs.foreach { eff2 => check(eff2)(state2) }
                }
              }
              else {
                traceIndented("!!!sym does not exist: " + pot.show)
              }

            case Warm(cls) =>
              val effs = theEnv.effectsOf(sym)
              val state2 = state.withVisited(eff)
              effs.foreach { eff =>
                val effs = substitute(eff, bindings, Some(cls -> pot))
                effs.foreach { eff2 => check(eff2)(state2) }
              }

            case Dependent(cls, bindings1) =>
              val effs = theEnv.effectsOf(sym)
              val state2 = state.withVisited(eff)
              effs.foreach { eff =>
                val effs = substitute(eff, bindings, Some(cls -> pot))
                effs.foreach { eff2 => check(eff2)(state2) }
              }

            case Fun(pots, effs) =>
              throw new Exception("Why I'm reached? " + pot.show)

            case pot =>
              val state2 = state.withVisited(eff)
              val pots = expand(pot)
              pots.foreach { pot =>
                check(DependentCall(pot, sym, bindings)(eff.source))(state2)
              }

          }
      }
    }

  private def expand(pot: Potential)(implicit state: State): Potentials =
    trace("expand " + pot.show, pots => Potentials.show(pots.asInstanceOf[Potentials])) { pot match {
      case MethodReturn(pot1, sym, virtual) =>
        pot1 match {
          case ThisRef(cls) =>
            assert(
              state.currentClass.derivesFrom(cls),
              "current class: " + state.currentClass.show + ", tp.symbol = " + cls
            )
            val targetSym = if (virtual) resolveVirtual(state.currentClass, sym) else sym
            if (targetSym.exists) {
              theEnv.potentialsOf(targetSym)
            }
            else {
              traceIndented("!!!sym does not exist: " + pot.show)
              Set.empty
            }

          case Fun(pots, effs) =>
            val name = sym.name.toString
            if (name == "apply") pots
            else if (name == "tupled") Set(pot1)
            else if (name == "curried") {
              val arity = defn.functionArity(sym.info.finalResultType)
              (1 until arity).foldLeft(Set(pot1)) { (acc, i) => Set(Fun(acc, Effects.empty)(pot1.source)) }
            }
            else Potentials.empty

          case Warm(cls) =>
            val targetSym = if (virtual) resolveVirtual(cls, sym) else sym
            val pots = theEnv.potentialsOf(targetSym)
            pots.flatMap { pot2 => substitute(pot2, Map.empty, Some(cls -> pot1)) }

          case Dependent(cls, bindings) =>
            val targetSym = if (virtual) resolveVirtual(cls, sym) else sym
            val pots = theEnv.potentialsOf(targetSym)
            pots.flatMap { pot2 => substitute(pot2, bindings, Some(cls -> pot1)) }

          case Cold(cls, _) =>
            Potentials.empty // error already reported, ignore

          case _ =>
            val (pots, effs) = expand(pot1).select(sym, pot.source, virtual)
            effs.foreach(check(_))
            pots
        }

      case FieldReturn(pot1, sym) =>
        pot1 match {
          case ThisRef(cls) =>
            // access to top-level objects
            val isPackage = cls.is(Flags.Package)
            if (!isPackage) assert(
              state.currentClass.derivesFrom(cls),
              "current class: " + state.currentClass.show + ", symbol = " + cls
            )
            if (isPackage) Set.empty
            else theEnv.potentialsOf(sym).map(devar(_, _ => None)).collect {
              case Some(pot) => pot
            }

          case Fun(pots, effs) =>
            throw new Exception("Unexpected code reached")

          case Warm(cls) =>
            def toCold(sym: Symbol): Potential = Cold(sym.info.classSymbol.asClass, definite = false)(pot.source)
            val pots = theEnv.potentialsOf(sym).map(devar(_, sym => Some(toCold(sym)))).collect {
              case Some(pot) => pot
            }
            pots.flatMap { pot2 => substitute(pot2, Map.empty, Some(cls -> pot1)) }

          case Dependent(cls, bindings) =>
            val pots = theEnv.potentialsOf(sym)
            pots.flatMap { pot2 => substitute(pot2, bindings, Some(cls -> pot1)) }

          case Cold(cls, _) =>
            Potentials.empty // error already reported, ignore

          case _ =>
            val (pots, effs) = expand(pot1).select(sym, pot.source)
            effs.foreach(check(_))
            pots
        }

      case DependentReturn(pot1, sym, bindings) =>
        pot1 match {
          case ThisRef(cls) =>
            // access to top-level objects
            val isPackage = cls.is(Flags.Package)
            if (!isPackage) assert(
              state.currentClass.derivesFrom(cls),
              "current class: " + state.currentClass.show + ", symbol = " + cls
            )
            if (isPackage) Set.empty
            else {
              val pots = theEnv.potentialsOf(sym)
              pots.flatMap { pot2 => substitute(pot2, bindings, Some(cls -> pot1)) }
            }

          case Fun(pots, effs) =>
            throw new Exception("Unexpected code reached")

          case Warm(cls) =>
            val pots = theEnv.potentialsOf(sym)
            pots.flatMap { pot2 => substitute(pot2, bindings, Some(cls -> pot1)) }

          case Dependent(cls, bindings1) =>
            val pots = theEnv.potentialsOf(sym)
            pots.flatMap { pot2 => substitute(pot2, bindings, Some(cls -> pot1)) }

          case Cold(cls, _) =>
            if (sym.isPrimaryConstructor) {
              val bindings2 = Summarization.dependentParamBindings(sym, bindings)
              Potentials.empty + Dependent(cls, bindings2)(pot.source)
            }
            else if (sym.isConstructor) {
              val pots = theEnv.potentialsOf(sym)
              assert(pots.size == 1, "pots = " + Potentials.show(pots))
              substitute(pots.head, bindings, None)
            }
            else {
              Potentials.empty // error already reported, ignore
            }

          case _ =>
            val (pots, effs) = expand(pot1).select(sym, pot.source, bindings = bindings)
            effs.foreach(check(_))
            pots
        }

      case Var(sym) =>
        assert(sym.owner.isPrimaryConstructor, "sym = " + sym.show)
        assert(
          state.currentClass.derivesFrom(sym.owner.owner),
          "current = " + state.currentClass.show + ", owner = " + sym.owner.owner.show
        )
        Potentials.empty

      case _: ThisRef | _: Fun | _: Warm | _: Cold | _: Dependent =>
        Set(pot)
    }
  }
}
