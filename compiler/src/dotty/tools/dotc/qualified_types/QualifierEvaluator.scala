package dotty.tools.dotc.qualified_types

import scala.annotation.tailrec

import dotty.tools.dotc.ast.tpd.{
  Apply,
  Block,
  ConstantTree,
  isIdempotentExpr,
  EmptyTree,
  Literal,
  Ident,
  Match,
  Select,
  This,
  Tree,
  TreeMap,
  ValDef,
  given
}
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Mode.Type
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.{defn, NoSymbol, Symbol}
import dotty.tools.dotc.core.SymDenotations.given
import dotty.tools.dotc.core.Types.{ConstantType, NoPrefix, TermRef}
import dotty.tools.dotc.inlines.InlineReducer
import dotty.tools.dotc.transform.TreeExtractors.BinaryOp
import dotty.tools.dotc.transform.patmat.{Empty as EmptySpace, SpaceEngine}
import dotty.tools.dotc.typer.Typer
import scala.util.boundary
import scala.util.boundary.break


import dotty.tools.dotc.reporting.trace
import dotty.tools.dotc.config.Printers

private[qualified_types] object QualifierEvaluator:
  /** Reduces a tree by constant folding, simplification and unfolding of simple
   *  references.
   *
   *  This is more aggressive than [[dotty.tools.dotc.transform.BetaReduce]] and
   *  [[dotty.tools.dotc.typer.ConstFold]] (which is used under the hood by
   *  `BetaReduce` through [[dotty.tools.dotc.ast.tpd.cpy]]), as it also unfolds
   *  non-constant expressions.
   */
  def evaluate(tree: Tree, args: Map[Symbol, Tree] = Map.empty)(using Context): Tree =
    trace(i"evaluate $tree", Printers.qualifiedTypes):
      QualifierEvaluator(args).transform(tree)

private class QualifierEvaluator(args: Map[Symbol, Tree]) extends TreeMap:
  import QualifierEvaluator.*

  override def transform(tree: Tree)(using Context): Tree =
    unfold(reduce(tree))

  private def reduce(tree: Tree)(using Context): Tree =
    tree match
      case tree: Apply =>
        val treeTransformed = super.transform(tree)
        constFold(treeTransformed).orElse(reduceBinaryOp(treeTransformed)).orElse(treeTransformed)
      case tree: Select =>
        val treeTransformed = super.transform(tree)
        constFold(treeTransformed).orElse(treeTransformed)
      case Block(Nil, expr) =>
        transform(expr)
      case tree =>
        super.transform(tree)

  private def constFold(tree: Tree)(using Context): Tree =
    tree match
      case ConstantTree(c: Constant) => Literal(c)
      case _                         => EmptyTree

  private def reduceBinaryOp(tree: Tree)(using Context): Tree =
    val d = defn // Need a stable path to match on `defn` members
    tree match
      case BinaryOp(a, d.Int_== | d.Any_== | d.Boolean_==, b) =>
        val aNormalized = QualifierNormalizer.normalize(a)
        val bNormalized = QualifierNormalizer.normalize(b)
        if QualifierAlphaComparer().iso(aNormalized, bNormalized) then
          Literal(Constant(true))
        else
          EmptyTree
      case _ =>
        EmptyTree

  private def unfold(tree: Tree)(using Context): Tree =
    args.get(tree.symbol) match
      case Some(tree2) =>
        return transform(tree2)
      case None => ()

    tree match
      case tree: Ident =>
        trace(s"unfold $tree", Printers.qualifiedTypes):
          tree.symbol.defTree match
            case valDef: ValDef
                if !valDef.rhs.isEmpty
                && !valDef.symbol.is(Flags.Lazy)
                && QualifiedTypes.isSimple(valDef.rhs) =>
              transform(valDef.rhs)
            case _ =>
              tree
      case _ =>
        tree
