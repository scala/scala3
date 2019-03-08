package dotty.tools
package dotc
package util

import scala.collection.mutable.ArrayBuffer
import dotty.tools.io._
import java.util.regex.Pattern
import java.io.IOException
import scala.tasty.util.Chars._
import Spans._
import scala.io.Codec
import core.Names.TermName
import core.Contexts.Context
import scala.annotation.internal.sharable
import core.Decorators.PreNamedString
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

import java.util.Optional

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
      } else 0
    new SourceFile(file, content drop headerLength) {
      override val underlying = new SourceFile(file, content)
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

  def this(file: AbstractFile, codec: Codec) = this(file, new String(file.toByteArray, codec.charSet).toCharArray)

  /** Tab increment; can be overridden */
  def tabInc: Int = 8

  override def name: String = file.name
  override def path: String = file.path
  override def jfile: Optional[JFile] = Optional.ofNullable(file.file)

  def pathName: PathName = file.absolutePath.toTermName

  override def equals(that: Any): Boolean =
    (this `eq` that.asInstanceOf[AnyRef]) || {
      that match {
        case that : SourceFile => file == that.file && start == that.start
        case _ => false
      }
    }

  override def hashCode: Int = file.hashCode * 41 + start.hashCode

  def apply(idx: Int): Char = content().apply(idx)

  def length: Int = content().length

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

  private def isLineBreak(idx: Int) =
    if (idx >= length) false else {
      val ch = content()(idx)
      // don't identify the CR in CR LF as a line break, since LF will do.
      if (ch == CR) (idx + 1 == length) || (content()(idx + 1) != LF)
      else isLineBreakChar(ch)
    }

  private def calculateLineIndices(cs: Array[Char]) = {
    val buf = new ArrayBuffer[Int]
    buf += 0
    for (i <- 0 until cs.length) if (isLineBreak(i)) buf += i + 1
    buf += cs.length // sentinel, so that findLine below works smoother
    buf.toArray
  }
  private lazy val lineIndices: Array[Int] = calculateLineIndices(content())

  /** Map line to offset of first character in line */
  def lineToOffset(index: Int): Int = lineIndices(index)

  /** Like `lineToOffset`, but doesn't crash if the index is out of bounds. */
  def lineToOffsetOpt(index: Int): Option[Int] =
    if (index < 0 || index >= lineIndices.length)
      None
    else
      Some(lineToOffset(index))

  /** A cache to speed up offsetToLine searches to similar lines */
  private[this] var lastLine = 0

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

  // Positioned ids

  private[this] val ctr = new AtomicInteger

  def nextId: Int = {
    val id = ctr.get
    if (id % ChunkSize == 0) newChunk
    else if (ctr.compareAndSet(id, id + 1)) id
    else nextId
  }

  private def newChunk: Int = sourceOfChunk.synchronized {
    val id = chunks << ChunkSizeLog
    if (chunks == sourceOfChunk.length) {
      val a = new Array[SourceFile](chunks * 2)
      System.arraycopy(sourceOfChunk, 0, a, 0, chunks)
      sourceOfChunk = a
    }
    sourceOfChunk(chunks) = this
    chunks += 1
    ctr.set(id + 1)
    id
  }
}
object SourceFile {
  implicit def eqSource: Eql[SourceFile, SourceFile] = Eql.derived

  implicit def fromContext(implicit ctx: Context): SourceFile = ctx.source

  type PathName = TermName

  def fromId(id: Int): SourceFile = sourceOfChunk(id >> ChunkSizeLog)

  def virtual(name: String, content: String) = new SourceFile(new VirtualFile(name, content.getBytes), scala.io.Codec.UTF8)

  private final val ChunkSizeLog = 10
  private final val ChunkSize = 1 << ChunkSizeLog

  // These two vars are sharable because they're only used in the synchronized block in newChunk
  @sharable private var chunks: Int = 0
  @sharable private var sourceOfChunk: Array[SourceFile] = new Array[SourceFile](2000)
}

@sharable object NoSource extends SourceFile(NoAbstractFile, Array[Char]()) {
  override def exists: Boolean = false
  override def atSpan(span: Span): SourcePosition = NoSourcePosition
}

