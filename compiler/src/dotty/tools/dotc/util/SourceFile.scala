package dotty.tools
package dotc
package util

import dotty.tools.io.*
import Spans.*
import core.Contexts.*
import core.Decorators.*

import scala.io.Codec
import Chars.*

import scala.annotation.internal.sharable
import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized
import dotty.tools.dotc.util.chaining.*

import java.io.File.separator
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{FileSystemException, Paths}
import java.util.Optional
import scala.annotation.threadUnsafe

object WrappedSourceFile:
  enum MagicHeaderInfo:
    case HasHeader(offset: Int, originalFile: SourceFile)
    case NoHeader
  import MagicHeaderInfo.*

  /** Convert a (source, span) pair into a SourcePosition, remapping through the
   *  magic offset header if applicable. */
  def sourcePos(sourceFile: SourceFile, span: Span)(using Context): SourcePosition =
    lookupMagicHeader(sourceFile) match
      case HasHeader(offset, originalFile) if span.exists && span.start >= offset =>
        originalFile.atSpan(span.shift(-offset))
      case _ => sourceFile.atSpan(span)

  private def lookupMagicHeader(sourceFile: SourceFile)(using Context): MagicHeaderInfo =
    val cache = ctx.base.magicHeaderCache
    cache.lookup(sourceFile) match
      case null =>
        val magicHeader = ctx.settings.YmagicOffsetHeader.value
        val result =
          if magicHeader.isEmpty then NoHeader
          else
            val text = new String(sourceFile.content)
            val headerQuoted = java.util.regex.Pattern.quote("///" + magicHeader)
            val regex = s"(?m)^$headerQuoted:(.+)$$".r
            regex.findFirstMatchIn(text) match
              case Some(m) =>
                val sourceStartOffset = sourceFile.nextLine(m.start)
                val name = m.group(1).nn
                val src = ctx.getSource(name)
                val file = src.file
                if file != null && file.exists then
                  HasHeader(sourceStartOffset, src)
                else
                  report.warning(em"original source file not found: $name")
                  NoHeader
              case None => NoHeader
        cache(sourceFile) = result
        result
      case result => result

class SourceFile (val file: AbstractFile | Null, sourceRoot: AbstractFile, codec: Codec) extends interfaces.SourceFile {
  private var myContent: Array[Char] | Null = null

  private var _maybeIncomplete: Boolean = false

  def maybeIncomplete: Boolean = _maybeIncomplete

  /** The contents of the original source file. Note that this can be empty, for example when
   * the source is read from Tasty. */
  def content(): Array[Char] =
    if file == null then Array.emptyCharArray
    else
      initialize(myContent, myContent = _,
        try new String(file.toByteArray, codec.charSet).toCharArray
        catch case _: FileSystemException => Array.empty[Char]
      )

  override def name: String =
    if file eq null then "" else file.name
  def ext: FileExtension =
    if file eq null then FileExtension.Empty else file.ext
  override def path: String =
    if file eq null then "" else file.path
  @threadUnsafe lazy val pathRelativeToSourceRoot: String =
    if (file eq null) || (file.jpath eq null) then
      throw new AssertionError(s"pathRelativeToSourceRoot called on a missing or non-disk file ('$path')")
    else if sourceRoot.jpath eq null then
      file.path
    else
      val sourcePath = file.jpath.nn.toAbsolutePath.normalize
      val refPath = sourceRoot.jpath.nn.toAbsolutePath.normalize
      if sourcePath.startsWith(refPath) then
        // On Windows we can only relativize paths if root component matches:
        //     try refPath.relativize(sourcePath).toString
        //     catch case _: IllegalArgumentException => sourcePath.toString
        // As we already check that the prefix matches, the special handling for
        // Windows is not needed.
        //
        // Also, consistently use '/' as separator so any path loaded from anywhere
        // is guaranteed to have the same separator, otherwise we'd see, e.g., "a\path",
        // and wonder "is this a Windows 2-part path, or a non-Windows file name with a backslash in it?"
        refPath.relativize(sourcePath).toString.replace(java.io.File.separatorChar, '/')
      else
        file.path

  override def jfile: Optional[JFile] =
    if file eq null then Optional.empty() else file.jfile

  override def equals(that: Any): Boolean =
    (this `eq` that.asInstanceOf[AnyRef]) || {
      that match {
        case that : SourceFile => file == that.file
        case _ => false
      }
    }

  override def hashCode: Int = if file eq null then 0 else file.hashCode

  def apply(idx: Int): Char = content().apply(idx)

  /** length of the original source file
   * Note that when the source is from Tasty, content() could be empty even though length > 0.
   * Use content().length to determine the length of content(). */
  def length: Int =
    if lineIndicesCache ne null then lineIndicesCache.last
    else content().length

  /** true for all source files except `NoSource` */
  def exists: Boolean = true

  def atSpan(span: Span): SourcePosition =
    if (span.exists) SourcePosition(this, span)
    else NoSourcePosition

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

  private var lineIndicesCache: Array[Int] = uninitialized
  private def lineIndices: Array[Int] =
    if lineIndicesCache eq null then
      lineIndicesCache = calculateLineIndicesFromContents()
    lineIndicesCache

  def initialized: Boolean = lineIndicesCache != null

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
    val idx = startOfLine(offset)
    offset - idx
  }

  /** The padding of the column corresponding to `offset`, includes tabs */
  def startColumnPadding(offset: Int): String = {
    var idx = startOfLine(offset)
    val pad = new StringBuilder
    while (idx != offset) {
      pad.append(if (idx < content().length && content()(idx) == '\t') '\t' else ' ')
      idx += 1
    }
    pad.result()
  }

  override def toString: String = 
    if file eq null then "<no file>" else file.toString
}
object SourceFile {
  implicit def fromContext(using Context): SourceFile = ctx.source

  /** A source file with an underlying virtual file. The path is taken as a file system path
   *  with the local separator converted to "/". The last element of the path will be the simple name of the file.
   */
  def virtual(name: String, content: String, maybeIncomplete: Boolean = false) =
    new SourceFile(new VirtualFile(name.replace(separator, "/"), content.getBytes(StandardCharsets.UTF_8)), new VirtualFile("_root_", Array.emptyByteArray), Codec.UTF8)
      .tap(_._maybeIncomplete = maybeIncomplete)

  /** A helper method to create a virtual source file for given URI.
   */
  def virtual(uri: URI, content: String): SourceFile =
    virtual(java.nio.file.Path.of(uri).toString, content)
}

@sharable object NoSource extends SourceFile(null, new VirtualFile("_root_", Array.emptyByteArray), Codec.UTF8) {
  override def exists: Boolean = false
  override def atSpan(span: Span): SourcePosition = NoSourcePosition
}
