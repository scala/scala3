package dotty.tools.dotc
package transform

import typer.Inliner
import core.Contexts.Context
import MegaPhase.MiniPhase

/** Drop Inlined nodes */
class DropInlined extends MiniPhase {
  import ast.tpd._
  override def phaseName = "dropInlined"

  override def transformInlined(tree: Inlined)(implicit ctx: Context): Tree =
    Inliner.dropInlined(tree)
}
