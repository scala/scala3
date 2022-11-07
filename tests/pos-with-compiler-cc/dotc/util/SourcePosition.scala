package dotty.tools
package dotc
package util

import scala.language.unsafeNulls

import printing.{Showable, Printer}
import printing.Texts._
import core.Contexts.Context
import Spans.{Span, NoSpan}
import scala.annotation.internal.sharable

/** A source position is comprised of a span and a source file */
case class SourcePosition(source: SourceFile, span: Span, outer: SourcePosition = NoSourcePosition)
extends SrcPos, interfaces.SourcePosition, Showable {

  def sourcePos(using Context) = this

  /** Is `that` a source position contained in this source position ?
   *  `outer` is not taken into account. */
  def contains(that: SourcePosition): Boolean =
    this.source == that.source && this.span.contains(that.span)

  def exists: Boolean = span.exists

  def lineContent: String = source.lineContent(point)

  def point: Int = span.point

  def line: Int = source.offsetToLine(point)

  /** Extracts the lines from the underlying source file as `Array[Char]`*/
  def linesSlice: Array[Char] =
    source.content.slice(source.startOfLine(start), source.nextLine(end))

  /** The lines of the position */
  def lines: Range = {
    val startOffset = source.offsetToLine(start)
    val endOffset = source.offsetToLine(end - 1) // -1 to drop a line if no chars in it form part of the position
    if (startOffset >= endOffset) line to line
    else startOffset to endOffset
  }

  def lineOffsets: List[Int] =
    lines.toList.map(source.lineToOffset(_))

  def beforeAndAfterPoint: (List[Int], List[Int]) =
    lineOffsets.partition(_ <= point)

  def column: Int = source.column(point)

  def start: Int = span.start
  def startLine: Int = source.offsetToLine(start)
  def startColumn: Int = source.column(start)
  def startColumnPadding: String = source.startColumnPadding(start)

  def end: Int = span.end
  def endLine: Int = source.offsetToLine(end)
  def endColumn: Int = source.column(end)

  def withOuter(outer: SourcePosition): SourcePosition = SourcePosition(source, span, outer)
  def withSpan(range: Span) = SourcePosition(source, range, outer)

  def startPos: SourcePosition = withSpan(span.startPos)
  def endPos  : SourcePosition = withSpan(span.endPos)
  def focus   : SourcePosition = withSpan(span.focus)
  def toSynthetic: SourcePosition = withSpan(span.toSynthetic)

  def outermost: SourcePosition =
    if outer == null || outer == NoSourcePosition then this else outer.outermost

  /** Inner most position that is contained within the `outermost` position.
   *  Most precise position that comes from the call site.
   */
  def nonInlined: SourcePosition = {
    val om = outermost
    def rec(self: SourcePosition): SourcePosition =
      if om.contains(self) then self else rec(self.outer)
    rec(this)
  }


  override def toString: String =
    s"${if (source.exists) source.file.toString else "(no source)"}:$span"

  def toText(printer: Printer): Text = printer.toText(this)
}

/** A sentinel for a non-existing source position */
@sharable object NoSourcePosition extends SourcePosition(NoSource, NoSpan, null) {
  override def line: Int = -1
  override def column: Int = -1
  override def toString: String = "?"
  override def withOuter(outer: SourcePosition): SourcePosition = outer
}

/** Things that can produce a source position and a span */
trait SrcPos:
  def sourcePos(using ctx: Context): SourcePosition
  def span: Span
  def startPos(using ctx: Context): SourcePosition = sourcePos.startPos
  def endPos(using ctx: Context): SourcePosition = sourcePos.endPos
  def focus(using ctx: Context): SourcePosition = sourcePos.focus
  def line(using ctx: Context): Int = sourcePos.line
