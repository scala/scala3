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
import java.util.regex.Pattern

object ScriptSourceFile {
  @sharable private val headerPattern = Pattern.compile("""^(::)?!#.*(\r|\n|\r\n)""", Pattern.MULTILINE)
  private val headerStarts  = List("#!", "::#!")

  /** Return true if has a script header */
  def hasScriptHeader(content: Array[Char]): Boolean =
    headerStarts.exists(content.startsWith(_))

  def apply(file: AbstractFile, content: Array[Char]): SourceFile = {
    /** Length of the script header from the given content, if there is one.
     *  The header begins with "#!" or "::#!" and is either a single line,
     *  or it ends with a line starting with "!#" or "::!#", if present.
     */
    val headerLength =
      if (headerStarts exists (content startsWith _)) {
        val matcher = headerPattern.matcher(content.mkString)
        if matcher.find then matcher.end
        else content.indexOf('\n') // end of first line
      }
      else 0

    // overwrite hash-bang lines with all spaces to preserve line numbers
    val hashBangLines = content.take(headerLength).mkString.split("\\r?\\n")
    if hashBangLines.nonEmpty then
      for i <- 0 until headerLength do
        content(i) match {
          case '\r' | '\n' =>
          case _ =>
            content(i) = ' '
        }

    new SourceFile(file, content) {
      override val underlying = new SourceFile(this.file, this.content)
    }
  }
}

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
                val file = ctx.getFile(m.group(1).nn)
                if file.exists then
                  HasHeader(sourceStartOffset, ctx.getSource(file))
                else
                  report.warning(em"original source file not found: ${file.path}")
                  NoHeader
              case None => NoHeader
        cache(sourceFile) = result
        result
      case result => result

class SourceFile(val file: AbstractFile, computeContent: => Array[Char]) extends interfaces.SourceFile {
  import SourceFile.*

  private var myContent: Array[Char] | Null = null

  /** The contents of the original source file. Note that this can be empty, for example when
   * the source is read from Tasty. */
  def content(): Array[Char] = {
    if (myContent == null) myContent = computeContent
    myContent.nn
  }

  private var _maybeInComplete: Boolean = false

  def maybeIncomplete: Boolean = _maybeInComplete

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

  /** length of the original source file
   * Note that when the source is from Tasty, content() could be empty even though length > 0.
   * Use content().length to determine the length of content(). */
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
    if isSelfContained then position // return the argument
    else SourcePosition(underlying, position.span.shift(start))

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

  def initialized = lineIndicesCache != null

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

  override def toString: String = file.toString
}
object SourceFile {
  implicit def eqSource: CanEqual[SourceFile, SourceFile] = CanEqual.derived

  implicit def fromContext(using Context): SourceFile = ctx.source

  /** A source file with an underlying virtual file. The name is taken as a file system path
   *  with the local separator converted to "/". The last element of the path will be the simple name of the file.
   */
  def virtual(name: String, content: String, maybeIncomplete: Boolean = false) =
    SourceFile(new VirtualFile(name.replace(separator, "/"), content.getBytes(StandardCharsets.UTF_8)), content.toCharArray)
      .tap(_._maybeInComplete = maybeIncomplete)

  /** A helper method to create a virtual source file for given URI.
   *  It relies on SourceFile#virtual implementation to create the virtual file.
   */
  def virtual(uri: URI, content: String): SourceFile =
    SourceFile(new VirtualFile(Paths.get(uri), content.getBytes(StandardCharsets.UTF_8)), content.toCharArray)

  /** Returns the relative path of `source` within the `reference` path
   *
   *  It returns the current path under `source.file.jpath` if it is not contained in `reference`.
   */
  def relativePath(source: SourceFile, reference: String): String = {
    val file = source.file
    val jpath = file.jpath
    if jpath eq null then
      file.path // repl and other custom tests use abstract files with no path
    else
      val sourcePath = jpath.toAbsolutePath.normalize
      val refPath = java.nio.file.Paths.get(reference).toAbsolutePath.normalize

      if sourcePath.startsWith(refPath) then
        // On Windows we can only relativize paths if root component matches
        // (see implementation of sun.nio.fs.WindowsPath#relativize)
        //
        //     try refPath.relativize(sourcePath).toString
        //     catch case _: IllegalArgumentException => sourcePath.toString
        //
        // As we already check that the prefix matches, the special handling for
        // Windows is not needed.

        // We also consistently use forward slashes as path element separators
        // for relative paths. If we didn't do that, it'd be impossible to parse
        // them back, as one would need to know whether they were created on Windows
        // and use both slashes as separators, or on other OS and use forward slash
        // as separator, backslash as file name character.

        import scala.jdk.CollectionConverters.*
        val path = refPath.relativize(sourcePath)
        path.iterator.asScala.mkString("/")
      else
        jpath.toString
  }

  /** Return true if file is a script:
   *  if filename extension is not .scala and has a script header.
   */
  def isScript(file: AbstractFile, content: Array[Char]): Boolean =
    ScriptSourceFile.hasScriptHeader(content)

  private def isUtf8Continuation(byte: Int): Boolean =
    (byte & 0xc0) == 0x80

  private def utf8DecodedLength(bytes: Array[Byte]): Int =
    var length = 0
    var i = 0
    while i < bytes.length do
      val b0 = bytes(i) & 0xff
      if b0 < 0x80 then
        length += 1
        i += 1
      else if b0 < 0xc2 then
        return -1
      else if b0 < 0xe0 then
        if i + 1 >= bytes.length then return -1
        val b1 = bytes(i + 1) & 0xff
        if !isUtf8Continuation(b1) then return -1
        length += 1
        i += 2
      else if b0 < 0xf0 then
        if i + 2 >= bytes.length then return -1
        val b1 = bytes(i + 1) & 0xff
        val b2 = bytes(i + 2) & 0xff
        val validB1 =
          if b0 == 0xe0 then 0xa0 <= b1 && b1 <= 0xbf
          else if b0 == 0xed then 0x80 <= b1 && b1 <= 0x9f
          else 0x80 <= b1 && b1 <= 0xbf
        if !validB1 || !isUtf8Continuation(b2) then return -1
        length += 1
        i += 3
      else if b0 < 0xf5 then
        if i + 3 >= bytes.length then return -1
        val b1 = bytes(i + 1) & 0xff
        val b2 = bytes(i + 2) & 0xff
        val b3 = bytes(i + 3) & 0xff
        val validB1 =
          if b0 == 0xf0 then 0x90 <= b1 && b1 <= 0xbf
          else if b0 == 0xf4 then 0x80 <= b1 && b1 <= 0x8f
          else 0x80 <= b1 && b1 <= 0xbf
        if !validB1 || !isUtf8Continuation(b2) || !isUtf8Continuation(b3) then return -1
        length += 2
        i += 4
      else
        return -1
    length

  private def decodeValidUtf8(bytes: Array[Byte], length: Int): Array[Char] =
    val chars = new Array[Char](length)
    var i = 0
    var j = 0
    while i < bytes.length do
      val b0 = bytes(i) & 0xff
      if b0 < 0x80 then
        chars(j) = b0.toChar
        i += 1
        j += 1
      else if b0 < 0xe0 then
        val ch = ((b0 & 0x1f) << 6) | (bytes(i + 1) & 0x3f)
        chars(j) = ch.toChar
        i += 2
        j += 1
      else if b0 < 0xf0 then
        val ch = ((b0 & 0x0f) << 12) | ((bytes(i + 1) & 0x3f) << 6) | (bytes(i + 2) & 0x3f)
        chars(j) = ch.toChar
        i += 3
        j += 1
      else
        val codePoint =
          ((b0 & 0x07) << 18) | ((bytes(i + 1) & 0x3f) << 12) | ((bytes(i + 2) & 0x3f) << 6) | (bytes(i + 3) & 0x3f)
        val shifted = codePoint - 0x10000
        chars(j) = (0xd800 + (shifted >>> 10)).toChar
        chars(j + 1) = (0xdc00 + (shifted & 0x3ff)).toChar
        i += 4
        j += 2
    chars

  private def decodeToChars(bytes: Array[Byte], codec: Codec): Array[Char] =
    val charset = codec.charSet
    if charset == StandardCharsets.UTF_8 then
      val length = utf8DecodedLength(bytes)
      if length >= 0 then decodeValidUtf8(bytes, length)
      else new String(bytes, charset).toCharArray
    else new String(bytes, charset).toCharArray

  private def decodedChars(file: AbstractFile, codec: Codec): Array[Char] =
    // Files.exists is slow on Java 8 (https://rules.sonarsource.com/java/tag/performance/RSPEC-3725),
    // so cope with failure.
    try decodeToChars(file.toByteArray, codec)
    catch
      case _: FileSystemException => Array.empty[Char]

  def apply(file: AbstractFile, codec: Codec): SourceFile =
    val chars = decodedChars(file, codec)

    if isScript(file, chars) then
      ScriptSourceFile(file, chars)
    else
      SourceFile(file, chars)

  def positionOnly(file: AbstractFile, lineSizes: Array[Int], codec: Codec): SourceFile =
    val source = SourceFile(file, SourceFile(file, codec).content())
    source.setLineIndicesFromLineSizes(lineSizes)
    source

  def apply(file: AbstractFile, computeContent: => Array[Char]): SourceFile = new SourceFile(file, computeContent)
}

@sharable object NoSource extends SourceFile(NoAbstractFile, Array[Char]()) {
  override def exists: Boolean = false
  override def atSpan(span: Span): SourcePosition = NoSourcePosition
}
