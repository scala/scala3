package dotty.tools.dotc.qualified_types

import dotty.tools.dotc.ast.tpd.{singleton, Apply, Block, Literal, Select, Tree, TreeMap, given}
import dotty.tools.dotc.core.Atoms
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.core.Symbols.{defn, Symbol}
import dotty.tools.dotc.core.Types.{ConstantType, TermRef}
import dotty.tools.dotc.config.Printers

import dotty.tools.dotc.reporting.trace
import dotty.tools.dotc.config.Printers

private[qualified_types] object QualifierNormalizer:
  def normalize(tree: Tree)(using Context): Tree =
    trace(i"normalize $tree", Printers.qualifiedTypes):
      QualifierNormalizer().transform(tree)

/** A [[TreeMap]] that normalizes trees by applying algebraic simplifications
 *  and by ordering operands.
 *
 *  Entry point: [[QualifierNormalizer.normalize]].
 */
private class QualifierNormalizer extends TreeMap:
  override def transform(tree: Tree)(using Context): Tree =
    val d = defn // Need a stable path to match on `defn` members
    tree match
      case Apply(method, _) =>
        method.symbol match
          case d.Int_+      => normalizeIntSum(tree)
          case d.Int_*      => normalizeIntProduct(tree)
          case _            => super.transform(tree)
      case _ => super.transform(tree)

  /** Normalizes a tree representing an integer sum.
   *
   *  The normalization consists in:
   *    - Grouping summands which have the same non-constant factors, such that
   *      `3x + x` becomes `4x` for example.
   *    - Sorting the summands, so that for example `x + y` and `y + x` are
   *      normalized to the same tree.
   *    - Simplifying `0 + x` to `x`.
   *    - Normalizing each summand using [[normalizeIntProduct]].
   */
  private def normalizeIntSum(tree: Tree)(using Context): Tree =
    val (summands, const) = decomposeIntSum(tree)
    makeIntSum(summands, const)

  /** Decomposes a tree representing an integer sum into a list of non-constant
   *  summands `s_i` and a constant `c`. The summands are grouped and sorted as
   *  described in [[normalizeIntSum]].
   */
  private def decomposeIntSum(tree: Tree)(using Context): (List[Tree], Int) =
    val groups: Map[List[QualifierStructuralComparer.TreeBox], Int] =
      getAllArguments(tree, defn.Int_+)
        .map(decomposeIntProduct)
        .groupMapReduce(_._1.map(QualifierStructuralComparer.TreeBox.apply))(_._2)(_ + _)
    val const = groups.getOrElse(Nil, 0)
    val summands =
      groups
        .filter((args, c) => c != 0 && !args.isEmpty)
        .toList
        .sortBy((pair: (List[QualifierStructuralComparer.TreeBox], Int)) => pair.hashCode())
        .map((args, c) => makeIntProduct(args.map(_.tree), c))
    (summands, const)

  /** Constructs a tree representing an integer sum from a list of non-constant
   *  summands `summands` and a constant `const`.
   */
  private def makeIntSum(summands: List[Tree], const: Int)(using Context): Tree =
    if summands.isEmpty then
      Literal(Constant(const))
    else
      val summandsTree = summands.reduce(_.select(defn.Int_+).appliedTo(_))
      if const == 0 then summandsTree
      else Literal(Constant(const)).select(defn.Int_+).appliedTo(summandsTree)

  /** Normalizes a tree representing an integer product.
   *
   *  The normalization consists in:
   *    - Sorting the factors, so that for example `x * y` and `y * x` are
   *      normalized to the same tree.
   *    - Simplifying `0 * x` to `0`.
   *    - Simplifying `1 * x` to `x`.
   */
  private def normalizeIntProduct(tree: Tree)(using Context): Tree =
    val (factors, const) = decomposeIntProduct(tree)
    makeIntProduct(factors, const)

  /** Decomposes a tree representing an integer product into a sorted list of
   *  non-constant factors `f_i` and a constant `c`.
   */
  private def decomposeIntProduct(tree: Tree)(using Context): (List[Tree], Int) =
    val (consts, factors) =
      getAllArguments(tree, defn.Int_*)
        .map(transform)
        .partitionMap:
          case Literal(Constant(n: Int)) => Left(n)
          case arg                       => Right(arg)
    (factors.sortBy(QualifierStructuralComparer.hash), consts.product)

  /** Constructs a tree representing an integer product from a sorted list of
   *  non-constant factors `factors` and a constant `const`.
   */
  private def makeIntProduct(factors: List[Tree], const: Int)(using Context): Tree =
    if const == 0 then
      Literal(Constant(0))
    else if factors.isEmpty then
      Literal(Constant(const))
    else
      val factorsTree = factors.reduce(_.select(defn.Int_*).appliedTo(_))
      if const == 1 then factorsTree
      else Literal(Constant(const)).select(defn.Int_*).appliedTo(factorsTree)

  /** Recursively collects all arguments of an n-ary operation.
   *
   *  For example, given the tree `(a + (b * c)) + (d + e)`, the method returns
   *  the list `[a, b * c, d, e]` when called with the `+` operator.
   */
  private def getAllArguments(tree: Tree, op: Symbol)(using Context): List[Tree] =
    tree match
      case Apply(method @ Select(qual, _), List(arg)) if method.symbol == op =>
        getAllArguments(qual, op) ::: getAllArguments(arg, op)
      case Block(Nil, expr) =>
        getAllArguments(expr, op)
      case _ =>
        List(transform(tree))
