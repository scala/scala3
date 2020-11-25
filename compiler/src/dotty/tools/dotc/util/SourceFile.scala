package dotty.tools
package dotc
package util

import dotty.tools.io._
import Spans._
import core.Contexts._

import scala.io.Codec
import Chars._
import scala.annotation.internal.sharable
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.util.Optional
import java.util.concurrent.atomic.AtomicInteger
import java.util.regex.Pattern

object ScriptSourceFile {
  @sharable private val headerPattern = Pattern.compile("""^(::)?!#.*(\r|\n|\r\n)""", Pattern.MULTILINE)
  private val headerStarts  = List("#!", "::#!")

  def apply(file: AbstractFile, content: Array[Char]): SourceFile = {
    /** Length of the script header from the given content, if there is one.
     *  The header begins with "#!" or "::#!" and ends with a line starting
     *  with "!#" or "::!#".
     */
    val headerLength =
      if (headerStarts exists (content startsWith _)) {
        val matcher = headerPattern matcher content.mkString
        if (matcher.find) matcher.end
        else throw new IOException("script file does not close its header with !# or ::!#")
      }
      else 0
    new SourceFile(file, content drop headerLength) {
      override val underlying = new SourceFile(this.file, this.content)
    }
  }
}

class SourceFile(val file: AbstractFile, computeContent: => Array[Char]) extends interfaces.SourceFile {
  import SourceFile._

  private var myContent: Array[Char] = null

  def content(): Array[Char] = {
    if (myContent == null) myContent = computeContent
    myContent
  }

  private var _maybeInComplete: Boolean = false

  def maybeIncomplete: Boolean = _maybeInComplete

  def this(file: AbstractFile, codec: Codec) =
    // It would be cleaner to check if the file exists instead of catching
    // an exception, but it turns out that Files.exists is remarkably slow,
    // at least on Java 8 (https://rules.sonarsource.com/java/tag/performance/RSPEC-3725),
    // this is significant enough to show up in our benchmarks.
    this(file,
      try new String(file.toByteArray, codec.charSet).toCharArray
      catch case _: java.nio.file.NoSuchFileException => Array[Char]())

  /** Tab increment; can be overridden */
  def tabInc: Int = 8

  override def name: String = file.name
  override def path: String = file.path
  override def jfile: Optional[JFile] = Optional.ofNullable(file.file)

  override def equals(that: Any): Boolean =
    (this `eq` that.asInstanceOf[AnyRef]) || {
      that match {
        case that : SourceFile => file == that.file && start == that.start
        case _ => false
      }
    }

  override def hashCode: Int = file.hashCode * 41 + start.hashCode

  def apply(idx: Int): Char = content().apply(idx)

  def length: Int =
    if lineIndicesCache ne null then lineIndicesCache.last
    else content().length

  /** true for all source files except `NoSource` */
  def exists: Boolean = true

  /** The underlying source file */
  def underlying: SourceFile = this

  /** The start of this file in the underlying source file */
  def start: Int = 0

  def atSpan(span: Span): SourcePosition =
    if (span.exists) SourcePosition(underlying, span)
    else NoSourcePosition

  def isSelfContained: Boolean = underlying eq this

  /** Map a position to a position in the underlying source file.
   *  For regular source files, simply return the argument.
   */
  def positionInUltimateSource(position: SourcePosition): SourcePosition =
    SourcePosition(underlying, position.span shift start)

  private def calculateLineIndicesFromContents() = {
    val cs = content()
    val buf = new ArrayBuffer[Int]
    buf += 0
    var i = 0
    while i < cs.length do
      val isLineBreak =
        val ch = cs(i)
        // don't identify the CR in CR LF as a line break, since LF will do.
        if ch == CR then i + 1 == cs.length || cs(i + 1) != LF
        else isLineBreakChar(ch)
      if isLineBreak then buf += i + 1
      i += 1
    buf += cs.length // sentinel, so that findLine below works smoother
    buf.toArray
  }

  private var lineIndicesCache: Array[Int] = _
  private def lineIndices: Array[Int] =
    if lineIndicesCache eq null then
      lineIndicesCache = calculateLineIndicesFromContents()
    lineIndicesCache
  def setLineIndicesFromLineSizes(sizes: Array[Int]): Unit =
    val lines = sizes.length
    val indices = new Array[Int](lines + 1)
    var i = 0
    val penultimate = lines - 1
    while i < penultimate do
      indices(i + 1) = indices(i) + sizes(i) + 1 // `+1` for the '\n' at the end of the line
      i += 1
    indices(lines) = indices(penultimate) + sizes(penultimate) // last line does not end with '\n'
    lineIndicesCache = indices

  /** Map line to offset of first character in line */
  def lineToOffset(index: Int): Int = lineIndices(index)

  /** Like `lineToOffset`, but doesn't crash if the index is out of bounds. */
  def lineToOffsetOpt(index: Int): Option[Int] =
    if (index < 0 || index >= lineIndices.length)
      None
    else
      Some(lineToOffset(index))

  /** A cache to speed up offsetToLine searches to similar lines */
  private var lastLine = 0

  /** Convert offset to line in this source file
   *  Lines are numbered from 0
   */
  def offsetToLine(offset: Int): Int = {
    lastLine = Util.bestFit(lineIndices, lineIndices.length, offset, lastLine)
    if (offset >= length) lastLine -= 1 // compensate for the sentinel
    lastLine
  }

  /** The index of the first character of the line containing position `offset` */
  def startOfLine(offset: Int): Int = {
    require(offset >= 0)
    lineToOffset(offsetToLine(offset))
  }

  /** The start index of the line following the one containing position `offset` */
  def nextLine(offset: Int): Int =
    lineToOffset(offsetToLine(offset) + 1 min lineIndices.length - 1)

  /** The content of the line containing position `offset` */
  def lineContent(offset: Int): String =
    content.slice(startOfLine(offset), nextLine(offset)).mkString

  /** The column corresponding to `offset`, starting at 0 */
  def column(offset: Int): Int = {
    var idx = startOfLine(offset)
    var col = 0
    while (idx != offset) {
      col += (if (idx < length && content()(idx) == '\t') (tabInc - col) % tabInc else 1)
      idx += 1
    }
    col
  }

  /** The padding of the column corresponding to `offset`, includes tabs */
  def startColumnPadding(offset: Int): String = {
    var idx = startOfLine(offset)
    val pad = new StringBuilder
    while (idx != offset) {
      pad.append(if (idx < length && content()(idx) == '\t') '\t' else ' ')
      idx += 1
    }
    pad.result()
  }

  override def toString: String = file.toString
}
object SourceFile {
  implicit def eqSource: CanEqual[SourceFile, SourceFile] = CanEqual.derived

  implicit def fromContext(using Context): SourceFile = ctx.source

  def virtual(name: String, content: String, maybeIncomplete: Boolean = false) =
    val src = new SourceFile(new VirtualFile(name, content.getBytes(StandardCharsets.UTF_8)), scala.io.Codec.UTF8)
    src._maybeInComplete = maybeIncomplete
    src

  def relativePath(source: SourceFile)(using Context): String = {
    def sourcerootPath =
      java.nio.file.Paths.get(ctx.settings.sourceroot.value)
      .toAbsolutePath
      .normalize
    val file = source.file
    val jpath = file.jpath
    if jpath eq null then
      file.path // repl and other custom tests use abstract files with no path
    else
      val normalizedPath = jpath.normalize
      // On Windows we can only relativize paths if root component matches
      // (see implementation of sun.nio.fs.WindowsPath#relativize)
      try sourcerootPath.relativize(normalizedPath).toString
      catch case _: IllegalArgumentException => normalizedPath.toString
  }
}

@sharable object NoSource extends SourceFile(NoAbstractFile, Array[Char]()) {
  override def exists: Boolean = false
  override def atSpan(span: Span): SourcePosition = NoSourcePosition
}

