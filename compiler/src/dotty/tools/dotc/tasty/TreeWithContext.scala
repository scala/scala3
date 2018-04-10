package dotty.tools.dotc.tasty

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

private[tasty] trait TreeWithContext {
  def tree: tpd.Tree
  def ctx: Context
}
