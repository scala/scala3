package dotty.tools.dotc
package transform

import core._
import DenotTransformers.SymTransformer
import Contexts._
import Flags._
import SymDenotations.SymDenotation
import collection.mutable
import MegaPhase.MiniPhase
import util.Store

/** Lift nested classes to toplevel */
class Flatten extends MiniPhase with SymTransformer {
  import ast.tpd._

  override def phaseName: String = Flatten.name

  override def description: String = Flatten.description

  // private[this] and protected[this] modifiers must be dropped
  // before classes are lifted. Getters drop these modifiers.
  override def runsAfter: Set[String] = Set(Getters.name)

  override def changesMembers: Boolean = true // the phase removes inner classes

  private var LiftedDefs: Store.Location[mutable.ListBuffer[Tree]] = _
  private def liftedDefs(using Context) = ctx.store(LiftedDefs)

  override def initContext(ctx: FreshContext): Unit =
    LiftedDefs = ctx.addLocation[mutable.ListBuffer[Tree]](null)

  def transformSym(ref: SymDenotation)(using Context): SymDenotation =
    if (ref.isClass && !ref.is(Package) && !ref.owner.is(Package))
      ref.copySymDenotation(
        name = ref.flatName,
        owner = ref.enclosingPackageClass)
    else ref

  override def prepareForPackageDef(tree: PackageDef)(using Context): FreshContext =
    ctx.fresh.updateStore(LiftedDefs, new mutable.ListBuffer[Tree])

  private def liftIfNested(tree: Tree)(using Context) =
    if (ctx.owner.is(Package)) tree
    else {
      transformFollowing(tree).foreachInThicket(liftedDefs += _)
      EmptyTree
    }

  override def transformStats(stats: List[Tree])(using Context): List[Tree] =
    if (ctx.owner.is(Package)) {
      val liftedStats = stats ++ liftedDefs
      liftedDefs.clear()
      liftedStats
    }
    else stats

  override def transformTypeDef(tree: TypeDef)(using Context): Tree =
    liftIfNested(tree)
}

object Flatten:
  val name: String = "flatten"
  val description: String = "lift all inner classes to package scope"
