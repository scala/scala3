package dotty.tools
package dotc
package util

import printing.{Showable, Printer}
import printing.Texts._
import Spans.{Span, NoSpan}
import scala.annotation.internal.sharable

/** A source position is comprised of a span and a source file */
case class SourcePosition(source: SourceFile, span: Span, outer: SourcePosition = NoSourcePosition)
extends interfaces.SourcePosition with Showable {

  /** Is `that` a source position contained in this source position ?
   *  `outer` is not taken into account. */
  def contains(that: SourcePosition): Boolean =
    this.source == that.source && this.span.contains(that.span)

  def exists: Boolean = span.exists

  def lineContent: String = source.lineContent(point)

  def point: Int = span.point

  def line: Int = if (source.exists) source.offsetToLine(point) else -1

  /** Extracts the lines from the underlying source file as `Array[Char]`*/
  def linesSlice: Array[Char] =
    source.content.slice(source.startOfLine(start), source.nextLine(end))

  /** The lines of the position */
  def lines: List[Int] = {
    val startOffset = source.offsetToLine(start)
    val endOffset = source.offsetToLine(end + 1)
    if (startOffset >= endOffset) line :: Nil
    else (startOffset until endOffset).toList
  }

  def lineOffsets: List[Int] =
    lines.map(source.lineToOffset(_))

  def beforeAndAfterPoint: (List[Int], List[Int]) =
    lineOffsets.partition(_ <= point)

  def column: Int = if (source.exists) source.column(point) else -1

  def start: Int = span.start
  def startLine: Int = if (source.exists) source.offsetToLine(start) else -1
  def startColumn: Int = if (source.exists) source.column(start) else -1
  def startColumnPadding: String = source.startColumnPadding(start)

  def end: Int = span.end
  def endLine: Int = if (source.exists) source.offsetToLine(end) else -1
  def endColumn: Int = if (source.exists) source.column(end) else -1

  def withOuter(outer: SourcePosition): SourcePosition = SourcePosition(source, span, outer)
  def withSpan(range: Span) = SourcePosition(source, range, outer)

  def startPos: SourcePosition = withSpan(span.startPos)
  def endPos  : SourcePosition = withSpan(span.endPos)
  def focus   : SourcePosition = withSpan(span.focus)
  def toSynthetic: SourcePosition = withSpan(span.toSynthetic)

  override def toString: String =
    s"${if (source.exists) source.file.toString else "(no source)"}:$span"

  def toText(printer: Printer): Text = printer.toText(this)
}

/** A sentinel for a non-existing source position */
@sharable object NoSourcePosition extends SourcePosition(NoSource, NoSpan, null) {
  override def toString: String = "?"
  override def withOuter(outer: SourcePosition): SourcePosition = outer
}

