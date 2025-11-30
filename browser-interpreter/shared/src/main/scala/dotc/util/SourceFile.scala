package dotc.util

import dotc.io.AbstractFile

/**
 * Cross-platform source file representation.
 *
 * This is a simplified version of dotty.tools.dotc.util.SourceFile.
 */
class SourceFile(val file: AbstractFile, val content: Array[Char]) {

  def this(file: AbstractFile) = this(file, file.toCharArray)

  def this(name: String, content: String) =
    this(AbstractFile(name, content), content.toCharArray)

  /** The source file name */
  def name: String = file.name

  /** The source file path */
  def path: String = file.path

  /** Length of the source */
  def length: Int = content.length

  /** Check bounds */
  def exists(offset: Int): Boolean = offset >= 0 && offset < content.length

  /** Get character at offset */
  def apply(offset: Int): Char =
    if (exists(offset)) content(offset) else '\u0000'

  /** Line starts (offsets where each line begins) */
  lazy val lineStarts: Array[Int] = {
    val buf = scala.collection.mutable.ArrayBuffer[Int](0)
    var i = 0
    while (i < content.length) {
      if (content(i) == '\n') buf += (i + 1)
      i += 1
    }
    buf.toArray
  }

  /** Number of lines */
  def lineCount: Int = lineStarts.length

  /** Convert offset to line number (0-based) */
  def offsetToLine(offset: Int): Int = {
    var lo = 0
    var hi = lineStarts.length - 1
    while (lo < hi) {
      val mid = (lo + hi + 1) / 2
      if (lineStarts(mid) <= offset) lo = mid
      else hi = mid - 1
    }
    lo
  }

  /** Convert offset to column number (0-based) */
  def offsetToColumn(offset: Int): Int = {
    val line = offsetToLine(offset)
    offset - lineStarts(line)
  }

  /** Convert line/column to offset */
  def lineColumnToOffset(line: Int, column: Int): Int = {
    if (line < 0 || line >= lineStarts.length) -1
    else lineStarts(line) + column
  }

  /** Get the start offset of a line */
  def lineStart(line: Int): Int =
    if (line >= 0 && line < lineStarts.length) lineStarts(line) else -1

  /** Get a line's content by line number (without newline) */
  def lineContent(line: Int): String = {
    if (line < 0 || line >= lineStarts.length) ""
    else {
      val start = lineStarts(line)
      val end = if (line + 1 < lineStarts.length) lineStarts(line + 1) - 1 else content.length
      new String(content, start, end - start)
    }
  }
  
  /** Get a line's content by offset (without newline) */
  def lineContentAt(offset: Int): String = lineContent(offsetToLine(offset))

  /** Get content in a range */
  def slice(start: Int, end: Int): String = {
    val s = math.max(0, start)
    val e = math.min(content.length, end)
    if (s < e) new String(content, s, e - s) else ""
  }

  /** Create a position in this source */
  def position(offset: Int): SourcePosition = SourcePosition(this, offset)

  /** Create a span in this source */
  def span(start: Int, end: Int): Span = Span(start, end)

  override def toString: String = s"SourceFile($path)"
}

/**
 * Companion object for SourceFile.
 */
object SourceFile {

  /** Create a source file from string content */
  def apply(name: String, content: String): SourceFile =
    new SourceFile(name, content)

  /** Create a source file from an AbstractFile */
  def apply(file: AbstractFile): SourceFile =
    new SourceFile(file)

  /** An empty/no source */
  val NoSource: SourceFile = new SourceFile("<no source>", "")
}

