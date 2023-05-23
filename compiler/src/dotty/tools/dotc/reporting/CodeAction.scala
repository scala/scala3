package dotty.tools.dotc.reporting

import dotty.tools.dotc.rewrites.Rewrites.ActionPatch

case class CodeAction(
  title: String,
  description: java.util.Optional[String],
  patches: java.util.List[ActionPatch]
)
