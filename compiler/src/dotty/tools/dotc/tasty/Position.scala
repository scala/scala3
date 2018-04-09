package dotty.tools.dotc.tasty

import dotty.tools.dotc.util.SourcePosition

private[tasty] class Position(val pos: SourcePosition) extends scala.tasty.Position {
  override def toString: String = s"Position(${pos.line}, ${pos.column})"
}
