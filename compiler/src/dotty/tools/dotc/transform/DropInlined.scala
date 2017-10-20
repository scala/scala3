package dotty.tools.dotc
package transform

import typer.Inliner
import core.Contexts.Context
import TreeTransforms.{MiniPhaseTransform, TransformerInfo}

/** Drop Inlined nodes */
class DropInlined extends MiniPhaseTransform {
  import ast.tpd._
  override def phaseName = "dropInlined"

  override def transformInlined(tree: Inlined)(implicit ctx: Context, info: TransformerInfo): Tree =
    Inliner.dropInlined(tree)
}
