package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.core.Decorators.sourcePos

private[tasty] trait Positioned extends scala.tasty.Positioned with TreeWithContext {
  def pos: scala.tasty.Position = new Position(sourcePos(tree.pos)(ctx))
}
