package dotty.tools
package dotc
package transform

import ast.Trees.*, ast.tpd, core.*
import Contexts.*, Types.*, Symbols.*
import StdNames.*, NameOps.*
import MegaPhase.MiniPhase
import inlines.Inliner.isElideableExpr

/** Specializes Tuples by replacing tuple construction and selection trees.
 *
 * Specifically:
 * 1. Replaces `(1, 1)` (which is `Tuple2.apply[Int, Int](1, 1)`) and
 *    `new Tuple2[Int, Int](1, 1)` with `new Tuple2$mcII$sp(1, 1)`.
 * 2. Replaces `(_: Tuple2[Int, Int])._1` with `(_: Tuple2[Int, Int])._1$mcI$sp`
 */
class SpecializeTuples extends MiniPhase:
  import tpd.*

  override def phaseName: String                 = SpecializeTuples.name
  override def description: String               = SpecializeTuples.description
  override def isEnabled(using Context): Boolean = !ctx.settings.scalajs.value

  override def transformApply(tree: Apply)(using Context): Tree = tree match
    case Apply(TypeApply(fun: NameTree, targs), args)
        if fun.symbol.name == nme.apply && fun.symbol.exists && defn.isSpecializableTuple(fun.symbol.owner.companionClass, targs.map(_.tpe))
        && isElideableExpr(tree)
    =>
      cpy.Apply(tree)(Select(New(defn.SpecializedTuple(fun.symbol.owner.companionClass, targs.map(_.tpe)).typeRef), nme.CONSTRUCTOR), args).withType(tree.tpe)
    case Apply(TypeApply(fun: NameTree, targs), args)
        if fun.symbol.name == nme.CONSTRUCTOR && fun.symbol.exists && defn.isSpecializableTuple(fun.symbol.owner, targs.map(_.tpe))
        && isElideableExpr(tree)
    =>
      cpy.Apply(tree)(Select(New(defn.SpecializedTuple(fun.symbol.owner, targs.map(_.tpe)).typeRef), nme.CONSTRUCTOR), args).withType(tree.tpe)
    case _ => tree
  end transformApply

  override def transformSelect(tree: Select)(using Context): Tree = tree match
    case Select(qual, name @ (nme._1 | nme._2)) =>
      qual.tpe.widenDealias match
        case AppliedType(tycon, args) if defn.isSpecializableTuple(tycon.classSymbol, args) =>
          val argIdx = if name == nme._1 then 0 else 1
          Select(qual, name.specializedName(args(argIdx) :: Nil))
        case _ =>
          tree
    case _ => tree
end SpecializeTuples

object SpecializeTuples:
  val name: String = "specializeTuples"
  val description: String = "replaces tuple construction and selection trees"
