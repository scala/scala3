package dotty.tools.dotc.qualified_types

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.{closureDef, singleton, Apply, Ident, Literal, Select, Tree, given}
import dotty.tools.dotc.config.Printers
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.{ctx, Context}
import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.core.Symbols.{defn, NoSymbol, Symbol}
import dotty.tools.dotc.core.Types.TermRef
import dotty.tools.dotc.reporting.trace
import dotty.tools.dotc.transform.BetaReduce

class QualifierSolver(using Context):
  val d = defn // Need a stable path to match on `defn` members

  def implies(tree1: Tree, tree2: Tree) =
    trace(i"implies $tree1 -> $tree2", Printers.qualifiedTypes):
      (tree1, tree2) match
        case (closureDef(defDef1), closureDef(defDef2)) =>
          val tree1ArgSym = defDef1.symbol.paramSymss.head.head
          val tree2ArgSym = defDef2.symbol.paramSymss.head.head
          val rhs = defDef1.rhs
          val lhs = defDef2.rhs
          if tree1ArgSym.info frozen_<:< tree2ArgSym.info then
            impliesRec1(rhs, lhs.subst(List(tree2ArgSym), List(tree1ArgSym)))
          else if tree2ArgSym.info frozen_<:< tree1ArgSym.info then
            impliesRec1(rhs.subst(List(tree1ArgSym), List(tree2ArgSym)), lhs)
          else
            false
        case _ =>
          throw IllegalArgumentException("Qualifiers must be closures")

  private def impliesRec1(tree1: Tree, tree2: Tree): Boolean =
    // tree1 = lhs || rhs
    tree1 match
      case Apply(select @ Select(lhs, name), List(rhs)) =>
        select.symbol match
          case d.Boolean_|| =>
            return impliesRec1(lhs, tree2) && impliesRec1(rhs, tree2)
          case _ => ()
      case _ => ()

    // tree2 = lhs && rhs, or tree2 = lhs || rhs
    tree2 match
      case Apply(select @ Select(lhs, name), List(rhs)) =>
        select.symbol match
          case d.Boolean_&& =>
            return impliesRec1(tree1, lhs) && impliesRec1(tree1, rhs)
          case d.Boolean_|| =>
            return impliesRec1(tree1, lhs) || impliesRec1(tree1, rhs)
          case _ => ()
      case _ => ()

    val egraph = EGraph(ctx)
    // println(s"tree implies $tree1 -> $tree2")
    (egraph.toNode(QualifierEvaluator.evaluate(tree1)), egraph.toNode(QualifierEvaluator.evaluate(tree2))) match
      case (Some(node1), Some(node2)) =>
        // println(s"node implies $node1 -> $node2")
        egraph.merge(node1, egraph.trueNode)
        egraph.repair()
        egraph.equiv(node2, egraph.trueNode)
      case _ =>
        false

  private def topLevelEqualities(tree: Tree): List[(Tree, Tree)] =
    trace(i"topLevelEqualities $tree", Printers.qualifiedTypes):
      topLevelEqualitiesImpl(tree)

  private def topLevelEqualitiesImpl(tree: Tree): List[(Tree, Tree)] =
    val d = defn
    tree match
      case Apply(select @ Select(lhs, name), List(rhs)) =>
        select.symbol match
          case d.Int_== | d.Any_== | d.Boolean_== => List((lhs, rhs))
          case d.Boolean_&&                       => topLevelEqualitiesImpl(lhs) ++ topLevelEqualitiesImpl(rhs)
          case _                                  => Nil
      case _ =>
        Nil
