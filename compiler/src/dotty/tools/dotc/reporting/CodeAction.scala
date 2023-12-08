package dotty.tools.dotc.reporting

import dotty.tools.dotc.rewrites.Rewrites.ActionPatch

/** A representation of a code action / fix that can be used by tooling to
  * apply a fix to their code.
  *
  * @param title The title of the fix, often showed to a user in their editor.
  * @param description An optional description of the fix.
  * @param patches The patches that this fix contains.
  */
case class CodeAction(
  title: String,
  description: Option[String],
  patches: List[ActionPatch]
)
