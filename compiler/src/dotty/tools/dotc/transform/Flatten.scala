package dotty.tools.dotc
package transform

import core._
import DenotTransformers.SymTransformer
import Contexts.{ContextRenamed, FreshContext}
import Flags._
import SymDenotations.SymDenotation
import collection.mutable
import MegaPhase.MiniPhase
import util.Store

/** Lift nested classes to toplevel */
class Flatten extends MiniPhase with SymTransformer {
  import ast.tpd._

  override def phaseName: String = "flatten"

  // private[this] and protected[this] modifiers must be dropped
  // before classes are lifted. Getters drop these modifiers.
  override def runsAfter: Set[String] = Set(Getters.name)

  override def changesMembers: Boolean = true // the phase removes inner classes

  private var LiftedDefs: Store.Location[mutable.ListBuffer[Tree]] = _
  private def liftedDefs(implicit ctx: ContextRenamed) = ctx.store(LiftedDefs)

  override def initContext(ctx: FreshContext): Unit =
    LiftedDefs = ctx.addLocation[mutable.ListBuffer[Tree]](null)

  def transformSym(ref: SymDenotation)(implicit ctx: ContextRenamed): SymDenotation = {
    if (ref.isClass && !ref.is(Package) && !ref.owner.is(Package)) {
      ref.copySymDenotation(
        name = ref.flatName,
        owner = ref.enclosingPackageClass)
    }
    else ref
  }

  override def prepareForPackageDef(tree: PackageDef)(implicit ctx: ContextRenamed): FreshContext =
    ctx.fresh.updateStore(LiftedDefs, new mutable.ListBuffer[Tree])

  private def liftIfNested(tree: Tree)(implicit ctx: ContextRenamed) =
    if (ctx.owner is Package) tree
    else {
      transformFollowing(tree).foreachInThicket(liftedDefs += _)
      EmptyTree
    }

  override def transformStats(stats: List[Tree])(implicit ctx: ContextRenamed): List[Tree] =
    if (ctx.owner is Package) {
      val liftedStats = stats ++ liftedDefs
      liftedDefs.clear()
      liftedStats
    }
    else stats

  override def transformTypeDef(tree: TypeDef)(implicit ctx: ContextRenamed): Tree =
    liftIfNested(tree)
}
