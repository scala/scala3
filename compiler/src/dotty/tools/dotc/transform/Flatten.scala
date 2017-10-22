package dotty.tools.dotc
package transform

import core._
import DenotTransformers.SymTransformer
import Phases.Phase
import Contexts.Context
import Flags._
import SymDenotations.SymDenotation
import collection.mutable
import SuperPhase.MiniPhase
import util.Property

object Flatten {
  import ast.tpd._
  val LiftedDefs = new Property.Key[mutable.ListBuffer[Tree]]
  def liftedDefs(implicit ctx: Context) = ctx.property(LiftedDefs).get
}

/** Lift nested classes to toplevel */
class Flatten extends MiniPhase with SymTransformer {
  import ast.tpd._
  import Flatten._

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

  override def prepareForPackageDef(tree: PackageDef)(implicit ctx: Context) =
    ctx.fresh.setProperty(LiftedDefs, new mutable.ListBuffer[Tree])

  private def liftIfNested(tree: Tree)(implicit ctx: Context) =
    if (ctx.owner is Package) tree
    else {
      transformFollowing(tree).foreachInThicket(liftedDefs += _)
      EmptyTree
    }

  override def transformStats(stats: List[Tree])(implicit ctx: Context) =
    if (ctx.owner is Package) {
      val liftedStats = stats ++ liftedDefs
      liftedDefs.clear()
      liftedStats
    }
    else stats

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context) =
    liftIfNested(tree)
}
