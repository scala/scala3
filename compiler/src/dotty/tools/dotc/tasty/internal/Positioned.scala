package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.core.Decorators.sourcePos

private[tasty] trait Positioned extends scala.tasty.Positioned with TreeWithContext {
  def pos(implicit ctx: scala.tasty.Context): scala.tasty.Position = {
    new Position(sourcePos(tree.pos)(ctx.asInstanceOf[TastyContext].ctx))
  }
}
