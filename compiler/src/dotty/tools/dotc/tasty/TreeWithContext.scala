package dotty.tools.dotc.tasty

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.core.Contexts.Context

private[tasty] trait TreeWithContext {
  def tree: Trees.Tree[_]
  def ctx: Context
}
