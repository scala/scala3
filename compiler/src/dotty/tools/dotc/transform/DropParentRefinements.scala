package dotty.tools.dotc.transform

import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.DenotTransformers.IdentityDenotTransformer
import dotty.tools.dotc.typer.Typer

object DropParentRefinements:
  val name: String = "dropParentRefinements"
  val description: String = "drop parent refinements from a template"

/** Drop parent refinements from a template. because they are generated without
  * an implementation. These refinements are unusally required for tracked
  * members with more specific types.
  */
class DropParentRefinements extends MiniPhase with IdentityDenotTransformer:
  thisPhase =>
  import tpd.*

  override def phaseName: String = DropParentRefinements.name

  override def description: String = DropParentRefinements.description

  override def runsAfterGroupsOf: Set[String] = Set(CountOuterAccesses.name)

  override def changesMembers: Boolean = true // the phase drops parent refinements

  override def transformTemplate(tree: tpd.Template)(using Context): tpd.Tree =
    val newBody = tree.body.filter(!_.hasAttachment(Typer.RefinementFromParent))
    tree.body.foreach { member =>
      if member.hasAttachment(Typer.RefinementFromParent) then
        member.symbol.dropAfter(thisPhase)
    }
    cpy.Template(tree)(body = newBody)
