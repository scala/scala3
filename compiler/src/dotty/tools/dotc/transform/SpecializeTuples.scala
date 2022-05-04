package dotty.tools
package dotc
package transform

import ast.Trees.*, ast.tpd, core.*
import Contexts.*, Types.*, Decorators.*, Symbols.*, DenotTransformers.*
import SymDenotations.*, Scopes.*, StdNames.*, NameOps.*, Names.*
import MegaPhase.MiniPhase

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
        if fun.name == nme.apply && fun.symbol.exists && defn.isSpecializableTuple(fun.symbol.owner.companionClass, targs.map(_.tpe)) =>
      Apply(Select(New(defn.SpecialisedTuple(fun.symbol.owner.companionClass, targs.map(_.tpe)).typeRef), nme.CONSTRUCTOR), args).withType(tree.tpe)
    case Apply(TypeApply(fun: NameTree, targs), args)
        if fun.name == nme.CONSTRUCTOR && fun.symbol.exists && defn.isSpecializableTuple(fun.symbol.owner, targs.map(_.tpe)) =>
      Apply(Select(New(defn.SpecialisedTuple(fun.symbol.owner, targs.map(_.tpe)).typeRef), nme.CONSTRUCTOR), args).withType(tree.tpe)
    case _ => tree
  end transformApply

  override def transformSelect(tree: Select)(using Context): Tree = tree match
    case Select(qual, nme._1) if qual.tpe.widen.match
      case AppliedType(tycon, args) => defn.isSpecializableTuple(tycon.classSymbol, args)
      case _                        => false
    => Select(qual, nme._1.specializedName(qual.tpe.widen.argInfos.slice(0, 1)))
    case Select(qual, nme._2) if qual.tpe.widen.match
      case AppliedType(tycon, args) => defn.isSpecializableTuple(tycon.classSymbol, args)
      case _                        => false
    => Select(qual, nme._2.specializedName(qual.tpe.widen.argInfos.slice(1, 2)))
    case _ => tree
end SpecializeTuples

object SpecializeTuples:
  val name: String = "specializeTuples"
  val description: String = "replaces tuple construction and selection trees"
