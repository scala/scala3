package dotty.tools.dotc
package transform

import core._
import DenotTransformers.SymTransformer
import Phases.Phase
import Contexts.Context
import Flags._
import SymDenotations.SymDenotation
import collection.mutable
import TreeTransforms.MiniPhaseTransform
import dotty.tools.dotc.transform.TreeTransforms.TransformerInfo

/** Lift nested classes to toplevel */
class Flatten extends MiniPhaseTransform with SymTransformer { thisTransform =>
  import ast.tpd._
  override def phaseName = "flatten"

  override def changesMembers = true // the phase removes inner classes

  def transformSym(ref: SymDenotation)(implicit ctx: Context) = {
    if (ref.isClass && !ref.is(Package) && !ref.owner.is(Package)) {
      ref.copySymDenotation(
        name = ref.flatName,
        owner = ref.enclosingPackageClass)
    }
    else ref
  }

  private val liftedDefs = new mutable.ListBuffer[Tree]

  private def liftIfNested(tree: Tree)(implicit ctx: Context, info: TransformerInfo) =
    if (ctx.owner is Package) tree
    else {
      transformFollowing(tree).foreachInThicket(liftedDefs += _)
      EmptyTree
    }

  override def transformStats(stats: List[Tree])(implicit ctx: Context, info: TransformerInfo) =
    if (ctx.owner is Package) {
      val liftedStats = stats ++ liftedDefs
      liftedDefs.clear()
      liftedStats
    }
    else stats

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context, info: TransformerInfo) =
    liftIfNested(tree)
}
