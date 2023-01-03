package dotty.tools.dotc
package transform

import core._
import MegaPhase.MiniPhase
import Contexts._
import Types._
import NameKinds.OuterSelectName

/** This phase rewrites outer selects `E.n_<outer>` which were introduced by
 *  inlining to outer paths.
 */
class ElimOuterSelect extends MiniPhase {
  import ast.tpd._

  override def phaseName: String = ElimOuterSelect.name

  override def description: String = ElimOuterSelect.description

  override def runsAfterGroupsOf: Set[String] = Set(ExplicitOuter.name)
    // ExplicitOuter needs to have run to completion before so that all classes
    // that need an outer accessor have one.

  /** Convert a selection of the form `qual.n_<outer>` to an outer path from `qual` of
   *  length `n`.
   */
  override def transformSelect(tree: Select)(using Context): Tree =
    tree.name match {
      case OuterSelectName(_, nhops) =>
        val SkolemType(tp) = tree.tpe: @unchecked
        ExplicitOuter.outer.path(start = tree.qualifier, count = nhops).ensureConforms(tp)
      case _ => tree
    }
}

object ElimOuterSelect:
  val name: String = "elimOuterSelect"
  val description: String = "expand outer selections"
