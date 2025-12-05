package dotc.util

/**
 * A span in a source file, represented as start and end offsets.
 */
case class Span(start: Int, end: Int) {

  /** Is this a valid span? */
  def exists: Boolean = start >= 0 && end >= start

  /** Is this the no-span sentinel? */
  def isZeroExtent: Boolean = start == end

  /** The length of this span */
  def length: Int = end - start

  /** Union of two spans */
  def union(other: Span): Span = {
    if (!exists) other
    else if (!other.exists) this
    else Span(math.min(start, other.start), math.max(end, other.end))
  }

  /** Check if this span contains an offset */
  def contains(offset: Int): Boolean = start <= offset && offset < end

  /** Check if this span contains another span */
  def contains(other: Span): Boolean = start <= other.start && other.end <= end

  /** Get a point span at the start */
  def startPos: Span = Span(start, start)

  /** Get a point span at the end */
  def endPos: Span = Span(end, end)

  /** Shift by an offset */
  def shift(delta: Int): Span = Span(start + delta, end + delta)

  /** Convert to a synthetic span (for generated code) */
  def toSynthetic: Span = this // Simplified - no synthetic flag

  override def toString: String = s"[$start..$end)"
}

object Span {
  /** No span */
  val NoSpan: Span = Span(-1, -1)

  /** A zero-extent span at an offset */
  def apply(offset: Int): Span = Span(offset, offset)
}

/**
 * A position in a source file with line/column information.
 */
case class SourcePosition(source: SourceFile, span: Span) {

  def this(source: SourceFile, offset: Int) = this(source, Span(offset, offset))

  /** The start offset */
  def start: Int = span.start

  /** The end offset */
  def end: Int = span.end

  /** Does this position exist? */
  def exists: Boolean = span.exists && source != null

  /** The line number (0-based) */
  def line: Int = if (exists) source.offsetToLine(start) else -1

  /** The column number (0-based) */
  def column: Int = if (exists) source.offsetToColumn(start) else -1

  /** The end line (0-based) */
  def endLine: Int = if (exists) source.offsetToLine(end) else -1

  /** The end column (0-based) */
  def endColumn: Int = if (exists) source.offsetToColumn(end) else -1

  /** Human-readable line number (1-based) */
  def lineNumber: Int = line + 1

  /** Human-readable column number (1-based) */
  def columnNumber: Int = column + 1

  /** The content of the line containing the start position */
  def lineContent: String = if (exists) source.lineContent(line) else ""

  /** The source file name */
  def sourceName: String = if (source != null) source.name else "<unknown>"

  /** The source file path */
  def sourcePath: String = if (source != null) source.path else "<unknown>"

  /** Create a position with a different span */
  def withSpan(newSpan: Span): SourcePosition = SourcePosition(source, newSpan)

  /** Create a position at the start of this one */
  def startPos: SourcePosition = withSpan(span.startPos)

  /** Create a position at the end of this one */
  def endPos: SourcePosition = withSpan(span.endPos)

  /** Format as "file:line:column" */
  def formatLocation: String = s"$sourceName:$lineNumber:$columnNumber"

  /** Format with context for error messages */
  def formatMessage(message: String): String = {
    val sb = new StringBuilder
    sb.append(s"$formatLocation: $message\n")
    if (exists) {
      sb.append(lineContent)
      sb.append("\n")
      sb.append(" " * column)
      sb.append("^")
      if (span.length > 1) {
        sb.append("~" * (math.min(span.length, lineContent.length - column) - 1))
      }
    }
    sb.toString
  }

  override def toString: String = formatLocation
}

object SourcePosition {

  /** Create a position from source and offset */
  def apply(source: SourceFile, offset: Int): SourcePosition =
    new SourcePosition(source, offset)

  /** No position */
  val NoSourcePosition: SourcePosition = SourcePosition(null, Span.NoSpan)
}

