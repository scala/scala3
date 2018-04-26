package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.util.SourcePosition

private[tasty] class Position(val pos: SourcePosition) extends scala.tasty.Position {
  override def start = pos.start
  override def end = pos.end

  override def sourceFile = pos.source.file.jpath

  override def startLine = pos.startLine
  override def endLine = pos.endLine

  override def startColumn = pos.startColumn
  override def endColumn = pos.endColumn

  override def toString: String = s"Position(${pos.line}, ${pos.column})"
}
