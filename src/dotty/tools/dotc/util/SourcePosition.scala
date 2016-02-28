package dotty.tools
package dotc
package util

import Positions.{Position, NoPosition}

/** A source position is comprised of a position in a source file */
case class SourcePosition(source: SourceFile, pos: Position) extends interfaces.SourcePosition {
  def exists = pos.exists

  def lineContent: String = source.lineContent(point)

  def point: Int = pos.point
  /** The line of the position, starting at 0 */
  def line: Int = source.offsetToLine(point)
  /** The column of the position, starting at 0 */
  def column: Int = source.column(point)

  def start: Int = pos.start
  def startLine: Int = source.offsetToLine(start)
  def startColumn: Int = source.column(start)

  def end: Int = pos.end
  def endLine: Int = source.offsetToLine(end)
  def endColumn: Int = source.column(end)

  override def toString =
    if (source.exists) s"${source.file}:${line + 1}"
    else s"(no source file, offset = ${pos.point})"
}

/** A sentinel for a non-existing source position */
@sharable object NoSourcePosition extends SourcePosition(NoSource, NoPosition) {
  override def toString = "?"
}

