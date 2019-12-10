package dotty.tools.dotc
package transform
package init

import core._
import Contexts.Context
import ast.tpd._
import Decorators._
import Symbols._
import Constants.Constant
import Types._
import config.Printers.init
import reporting.trace

import Effects._, Potentials._, Summary._, Util._

object Summarization {
  private val ignoredMethods = Set(
    // "dotty.runtime.LazyVals$.setFlag",
    // "dotty.runtime.LazyVals$.get",
    // "dotty.runtime.LazyVals$.CAS",
    // "dotty.runtime.LazyVals$.wait4Notification",
    "scala.runtime.EnumValues.register",
    "java.lang.Object.isInstanceOf",
    "java.lang.Object.getClass",
    "java.lang.Object.eq",
    "java.lang.Object.ne"
  )

  /** Summarization of potentials and effects for an expression
   *
   *  Optimization:
   *
   *   1. potentials for expression of primitive value types can be
   *      safely abandoned, as they are always fully initialized.
   */
  def analyze(expr: Tree)(implicit ctx: Context): Summary =
  trace("summarizing " + expr.show, init, s => Summary.show(s.asInstanceOf[Summary])) {
    val summary: Summary = expr match {
      case Ident(name) =>
        assert(name.isTermName, "type trees should not reach here")
        analyze(expr.tpe, expr)

      case supert: Super =>
        val SuperType(thisTp, superTp) = supert.tpe.asInstanceOf[SuperType]
        val pot = SuperRef(thisTp.classSymbol.asClass, superTp.classSymbol.asClass)(supert)
        Summary.empty + pot

      case Select(qualifier, name) =>
        val (pots, effs) = analyze(qualifier)
        val (pots2, effs2) = pots.select(expr.symbol, expr)
        (pots2, effs ++ effs2)

      case _: This =>
        val cls = expr.tpe.widen.classSymbol.asClass
        Summary.empty + ThisRef(cls)(expr)

      case Apply(fun, args) =>
        val summary = analyze(fun)
        args.foldLeft(summary) { (sum, arg) =>
          val (pots1, effs1) = analyze(arg)
          sum.withEffs(pots1.leak(arg) ++ effs1)
        }

      case TypeApply(fun, args) =>
        analyze(fun)

      case Literal(const) =>
        Summary.empty

      case New(tpt) =>
        ???

      case Typed(expr, tpt) =>
        analyze(expr)

      case NamedArg(name, arg) =>
        analyze(arg)

      case Assign(lhs, rhs) =>
        val (pots, effs) = analyze(rhs)
        (Potentials.empty, pots.leak(expr) ++ effs)

      case closureDef(ddef) =>     // must be before `Block`
        analyze(ddef.rhs)

      case Block(stats, expr) =>
        val effs = stats.foldLeft(Effects.empty) { (acc, stat) => acc ++ analyze(stat)._2 }
        val (pots2, effs2) = analyze(expr)
        (pots2, effs ++ effs2)

      case If(cond, thenp, elsep) =>
        val (pots0, effs0) = analyze(cond)
        val (pots1, effs1) = analyze(thenp)
        val (pots2, effs2) = analyze(elsep)
        (pots0 ++ pots1 ++ pots2, effs0 ++ effs1 ++ effs2)

      case Annotated(arg, annot) =>
        if (expr.tpe.hasAnnotation(defn.UncheckedAnnot)) Summary.empty
        else analyze(arg)

      case Match(selector, cases) =>
        // possible for switches
        val (pots, effs) = analyze(selector)
        cases.foldLeft((Potentials.empty, pots.leak(selector) ++ effs)) { (acc, cas) =>
          acc union analyze(cas.body)
        }

      // case CaseDef(pat, guard, body) =>
      //   Summary.empty

      case Return(expr, from) =>
        // TODO: return potential to the method
        analyze(expr)

      case WhileDo(cond, body) =>
        // for lazy fields, the translation may result im `while (<empty>)`
        val (_, effs1) = if (cond.isEmpty) Summary.empty else analyze(cond)
        val (_, effs2) = analyze(body)
        (Potentials.empty, effs1 ++ effs2)

      case Labeled(_, expr) =>
        val (_, effs1) = analyze(expr)
        (Potentials.empty, effs1)

      case Try(block, cases, finalizer) =>
        val (pots, effs) =  cases.foldLeft(analyze(block)) { (acc, cas) =>
          acc union analyze(cas.body)
        }
        val (_, eff2) = if (finalizer.isEmpty) Summary.empty else analyze(finalizer)
        (pots, effs ++ eff2)

      case SeqLiteral(elems, elemtpt) =>
        val effsAll: Effects = elems.foldLeft(Effects.empty) { (effs, elem) =>
          val (pots1, effs1) = analyze(elem)
          pots1.leak(expr) ++ effs1 ++ effs
        }
        (Potentials.empty, effsAll)

      case Inlined(call, bindings, expansion) =>
        // TODO: bindings
        analyze(expansion)

      case tree @ ValDef(name, tpt, _) =>
        assert(!tree.symbol.owner.isClass, "unexpected ValDef")
        val (pots, effs) = analyze(tree.rhs)
        (Potentials.empty, pots.leak(expr) ++ effs)

      case Thicket(List()) =>
        // possible in try/catch/finally, see tests/crash/i6914.scala
        Summary.empty

      case _ =>
        throw new Exception("unexpected tree: " + expr.show)
    }

    if (isDefiniteHot(expr.tpe)) (Potentials.empty, summary._2)
    else summary
  }

  private def isDefiniteHot(tp: Type)(implicit ctx: Context): Boolean = {
    val sym = tp.widen.finalResultType.typeSymbol
    sym.isPrimitiveValueClass || sym == defn.StringClass
  }

  def analyze(tp: Type, source: Tree)(implicit ctx: Context): Summary =
  trace("summarizing " + tp.show, init, s => Summary.show(s.asInstanceOf[Summary])) {
    val summary: Summary = tp match {
      case tmref: TermRef if tmref.prefix == NoPrefix =>
        Summary.empty
      case tmref: TermRef =>
        val (pots, effs) = analyze(tmref.prefix, source)
        val (pots2, effs2) = pots.select(tmref.symbol, source)
        (pots2, effs ++ effs2)
      case ThisType(tref: TypeRef) if tref.classSymbol.is(Flags.Package) =>
        Summary.empty
      case thisTp: ThisType =>
        val cls = thisTp.widen.classSymbol.asClass
        Summary.empty + ThisRef(cls)(source)
      case SuperType(thisTp, superTp) =>
        val pot = SuperRef(thisTp.classSymbol.asClass, superTp.classSymbol.asClass)(source)
        Summary.empty + pot
      }

    if (isDefiniteHot(tp)) (Potentials.empty, summary._2)
    else summary
  }

  /** Constructors need special treatment to distinguish assignment from initialization
   *
   *  This is possible thanks to the convention in Dotty that assignment will be method
   *  calls, while field initialization will be assignments at the phase genBCode.
   */
  def analyzeConstructor(ctor: Symbol, tree: Tree)(implicit ctx: Context): Summary =
  trace("summarizing constructor" + tree.show, init, s => Summary.show(s.asInstanceOf[Summary])) {
    val block = tree.asInstanceOf[Block]
    val effsAll = (block.expr :: block.stats).foldLeft(Effects.empty) { case (acc, stat) =>
      stat match {
        case Assign(lhs, rhs) =>
          // no leaking for initialization
          val (_, effs) = analyze(rhs)
          acc ++ effs

        case tree =>
          val (_, effs) = analyze(tree)
          acc ++ effs
      }
    }
    val pots =
      if (ctor.isPrimaryConstructor) Potentials.empty
      else analyze(block.stats(0))._1

    (pots, effsAll)
  }

  def getRhs(symbol: Symbol)(implicit ctx: Context): Tree = {
    def notFound() = {
      traceIndented("Not tree found for " + symbol, init)
      EmptyTree
    }

    if (symbol.isAllOf(Flags.Lazy | Flags.Method, butNot = Flags.Lifted)) {
      if (symbol.defTree.isEmpty) {
        notFound()
        return symbol.defTree
      }

      var rhs: Tree = symbol.defTree.asInstanceOf[DefDef].rhs
      symbol.defTree.foreachSubTree {
        case vdef: ValDef if vdef.symbol.is(Flags.Synthetic) && vdef.name.toString == "result" =>
          rhs = vdef.rhs
        case _ =>
      }

      rhs
    }
    else if (symbol.is(Flags.Method))
      // give up if cross project boundary
      if (symbol.defTree.isEmpty) notFound()
      else symbol.defTree.asInstanceOf[DefDef].rhs
    else if (symbol.isAllOf(Flags.Module | Flags.StableRealizable, butNot = Flags.Method))
      // static fields
      EmptyTree
    else // rhs for fields are `assign` in ctor of its definition class
      symbol.owner.defTree match {
        case TypeDef(_, Template(ctor, _, _, _)) =>
          ctor.rhs match {
            case Block(stats, expr) =>
              stats.collectFirst {
                case Assign(sel @ Select(_: This, _), rhs) if sel.symbol == symbol => rhs
                case Assign(ident, rhs) if ident.symbol == symbol => rhs
              } match {
                case Some(init) => init
                case None =>
                  notFound()
              }
            case _ =>
              notFound()
          }
      }
  }
}
