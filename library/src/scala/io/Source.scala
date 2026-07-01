/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package io

import scala.collection.{AbstractIterator, BufferedIterator}
import java.io.{Closeable, FileInputStream, FileNotFoundException, InputStream, PrintStream, File => JFile}
import java.net.{URI, URL}

import scala.language.`2.13`
import scala.annotation.nowarn

/** This object provides convenience methods to create an iterable
 *  representation of a source file.
 */
object Source {
  val DefaultBufSize = 2048

  /** Creates a `Source` from System.in. */
  def stdin = fromInputStream(System.in)

  /** Creates a Source from an Iterable.
   *
   *  @param    iterable  the Iterable
   *  @return   the Source
   */
  def fromIterable(iterable: Iterable[Char]): Source = new Source {
    val iter = iterable.iterator
  } withReset(() => fromIterable(iterable))

  /** Creates a Source instance from a single character.
   *
   *  @param c the character to use as the source content
   */
  def fromChar(c: Char): Source = fromIterable(Array(c))

  /** Creates Source from array of characters, with empty description.
   *
   *  @param chars the array of characters to use as the source content
   */
  def fromChars(chars: Array[Char]): Source = fromIterable(chars)

  /** Creates Source from a String, with no description.
   *
   *  @param s the string to use as the source content
   */
  def fromString(s: String): Source = fromIterable(s)

  /** Creates Source from file with given name, setting its description to
   *  filename.
   *
   *  @param name the name of the file to read
   *  @param codec the implicit codec used for character encoding
   */
  def fromFile(name: String)(implicit codec: Codec): BufferedSource =
    fromFile(new JFile(name))(using codec)

  /** Creates Source from file with given name, using given encoding, setting
   *  its description to filename.
   *
   *  @param name the name of the file to read
   *  @param enc the name of the character encoding to use
   */
  def fromFile(name: String, enc: String): BufferedSource =
    fromFile(name)(using Codec(enc))

  /** Creates `source` from file with given file `URI`.
   *
   *  @param uri the file URI to read from
   *  @param codec the implicit codec used for character encoding
   */
  def fromFile(uri: URI)(implicit codec: Codec): BufferedSource =
    fromFile(new JFile(uri))(using codec)

  /** Creates Source from file with given file: URI
   *
   *  @param uri the file URI to read from
   *  @param enc the name of the character encoding to use
   */
  def fromFile(uri: URI, enc: String): BufferedSource =
    fromFile(uri)(using Codec(enc))

  /** Creates Source from file, using default character encoding, setting its
   *  description to filename.
   *
   *  @param file the file to read from
   *  @param codec the implicit codec used for character encoding
   */
  def fromFile(file: JFile)(implicit codec: Codec): BufferedSource =
    fromFile(file, Source.DefaultBufSize)(using codec)

  /** same as fromFile(file, enc, Source.DefaultBufSize)
   *
   *  @param file the file to read from
   *  @param enc the name of the character encoding to use
   */
  def fromFile(file: JFile, enc: String): BufferedSource =
    fromFile(file)(using Codec(enc))

  /** Creates a `Source` from `file`, using the named character encoding, with
   *  input buffered in a buffer of size `bufferSize`, setting its description to
   *  the filename.
   *
   *  @param file the file to read from
   *  @param enc the name of the character encoding to use
   *  @param bufferSize the size of the input buffer, in characters
   */
  def fromFile(file: JFile, enc: String, bufferSize: Int): BufferedSource =
    fromFile(file, bufferSize)(using Codec(enc))

  /** Creates Source from `file`, using given character encoding, setting
   *  its description to filename. Input is buffered in a buffer of size
   *  `bufferSize`.
   *
   *  @param file the file to read from
   *  @param bufferSize the size of the input buffer, in characters
   *  @param codec the implicit codec used for character encoding
   */
  def fromFile(file: JFile, bufferSize: Int)(implicit codec: Codec): BufferedSource = {
    val inputStream = new FileInputStream(file)

    createBufferedSource(
      inputStream,
      bufferSize,
      () => fromFile(file, bufferSize)(using codec),
      () => inputStream.close()
    )(using codec) withDescription s"file:${file.getAbsolutePath}"
  }

  /** Creates a `Source` from array of bytes, decoding
   *  the bytes according to codec.
   *
   *  @param bytes the array of bytes to decode into characters
   *  @param codec the implicit codec used for character encoding
   *  @return the created `Source` instance
   */
  def fromBytes(bytes: Array[Byte])(implicit codec: Codec): Source =
    fromString(new String(bytes, codec.name))

  /** Creates a `Source` from array of bytes, decoding the bytes according to
   *  the named character encoding.
   *
   *  @param bytes the array of bytes to decode into characters
   *  @param enc the name of the character encoding to use
   *  @return the created `Source` instance
   */
  def fromBytes(bytes: Array[Byte], enc: String): Source =
    fromBytes(bytes)(using Codec(enc))

  /** Creates a `Source` from array of bytes, assuming one byte per character
   *  (ISO-8859-1 encoding).
   *
   *  @param bytes the array of bytes to decode into characters
   *  @return the created `Source` instance
   */
  @deprecated("Use `fromBytes` and specify an encoding", since="2.13.9")
  def fromRawBytes(bytes: Array[Byte]): Source =
    fromString(new String(bytes, Codec.ISO8859.charSet))

  /** creates `Source` from file with given file: URI
   *
   *  @param uri the file URI to read from
   *  @param codec the implicit codec used for character encoding
   */
  def fromURI(uri: URI)(implicit codec: Codec): BufferedSource =
    fromFile(new JFile(uri))(using codec)

  /** Same as fromURL(new URL(s))(Codec(enc))
   *
   *  @param s the URL string to read from
   *  @param enc the name of the character encoding to use
   */
  def fromURL(s: String, enc: String): BufferedSource =
    fromURL(s)(using Codec(enc))

  /** Same as fromURL(new URL(s))
   *
   *  @param s the URL string to read from
   *  @param codec the implicit codec used for character encoding
   */
  def fromURL(s: String)(implicit codec: Codec): BufferedSource =
    fromURL(new URI(s).toURL)(using codec)

  /** Same as fromInputStream(url.openStream())(Codec(enc))
   *
   *  @param url the URL to read from
   *  @param enc the name of the character encoding to use
   */
  def fromURL(url: URL, enc: String): BufferedSource =
    fromURL(url)(using Codec(enc))

  /** Same as fromInputStream(url.openStream())(codec)
   *
   *  @param url the URL to read from
   *  @param codec the implicit codec used for character encoding
   */
  def fromURL(url: URL)(implicit codec: Codec): BufferedSource =
    fromInputStream(url.openStream())(using codec)

  /** Reads data from inputStream with a buffered reader, using the encoding
   *  in implicit parameter codec.
   *
   *  @param  inputStream  the input stream from which to read
   *  @param  bufferSize   buffer size (defaults to Source.DefaultBufSize)
   *  @param  reset        a () => Source which resets the stream (if unset, reset() will throw an Exception)
   *  @param  close        a () => Unit method which closes the stream (if unset, close() will do nothing)
   *  @param  codec        (implicit) a scala.io.Codec specifying behavior (defaults to Codec.default)
   *  @return              the buffered source
   */
  def createBufferedSource(
    inputStream: InputStream,
    bufferSize: Int = DefaultBufSize,
    reset: (() => Source) | Null = null,
    close: (() => Unit) | Null = null
  )(implicit codec: Codec): BufferedSource = {
    // workaround for default arguments being unable to refer to other parameters
    val resetFn = if (reset == null) () => createBufferedSource(inputStream, bufferSize, reset, close)(using codec) else reset

    new BufferedSource(inputStream, bufferSize)(using codec) withReset resetFn withClose close
  }

  /** Creates a `Source` reading from an input stream, using the named character
   *  encoding.
   *
   *  @param is the input stream from which to read
   *  @param enc the name of the character encoding to use
   *  @return the buffered source
   */
  def fromInputStream(is: InputStream, enc: String): BufferedSource =
    fromInputStream(is)(using Codec(enc))

  /** Creates a `Source` reading from an input stream, using the encoding
   *  specified by `codec`.
   *
   *  @param is the input stream from which to read
   *  @param codec the implicit codec used for character encoding
   *  @return the buffered source
   */
  def fromInputStream(is: InputStream)(implicit codec: Codec): BufferedSource =
    createBufferedSource(is, reset = () => fromInputStream(is)(using codec), close = () => is.close())(using codec)

  /** Reads data from a classpath resource, using either a context classloader (default) or a passed one.
   *
   *  @param  resource     name of the resource to load from the classpath
   *  @param  classLoader  classloader to be used, or context classloader if not specified
   *  @return              the buffered source
   */
  def fromResource(resource: String, classLoader: ClassLoader = Thread.currentThread().getContextClassLoader())(implicit codec: Codec): BufferedSource =
    Option(classLoader.getResourceAsStream(resource)) match {
      case Some(in) => fromInputStream(in)
      case None     => throw new FileNotFoundException(s"resource '$resource' was not found in the classpath from the given classloader.")
    }

}

/** An iterable representation of source data.
 *  It may be reset with the optional [[reset]] method.
 *
 *  Subclasses must supply [[scala.io.Source.iter the underlying iterator]].
 *
 *  Error handling may be customized by overriding the [[scala.io.Source.report report]] method.
 *
 *  The [[scala.io.Source.ch current input]] and [[scala.io.Source.pos position]],
 *  as well as the [[scala.io.Source.next next character]] methods delegate to
 *  [[scala.io.Source#Positioner the positioner]].
 *
 *  The default positioner encodes line and column numbers in the position passed to [[report]].
 *  This behavior can be changed by supplying a
 *  [[scala.io.Source.withPositioning(pos:* custom positioner]].
 */
abstract class Source extends Iterator[Char] with Closeable {
  /** The actual iterator. */
  protected val iter: Iterator[Char]

  // ------ public values

  /** Description of this source, default empty. */
  var descr: String = ""
  var nerrors = 0
  var nwarnings = 0

  private def lineNum(line: Int): String = (getLines() drop (line - 1) take 1).mkString

  /** An iterator over the lines of this source, excluding any line separator
   *  characters.
   */
  class LineIterator extends AbstractIterator[String] with Iterator[String] {
    private val sb = new StringBuilder

    lazy val iter: BufferedIterator[Char] = Source.this.iter.buffered
    /** Returns `true` if `ch` is a carriage return or line feed character.
     *
     *  @param ch the character to test
     */
    def isNewline(ch: Char): Boolean = ch == '\r' || ch == '\n'
    /** Returns `true` after appending the next non-terminating character to the
     *  current line buffer, or `false` at a line separator or end of input.
     */
    def getc(): Boolean = iter.hasNext && {
      val ch = iter.next()
      if (ch == '\n') false
      else if (ch == '\r') {
        if (iter.hasNext && iter.head == '\n')
          iter.next()

        false
      }
      else {
        sb append ch
        true
      }
    }
    /** Returns `true` if this source has more lines to read. */
    def hasNext: Boolean = iter.hasNext
    /** Returns the next line, excluding its line separator. */
    def next(): String = {
      sb.clear()
      while (getc()) { }
      sb.toString
    }
  }

  /** Returns an iterator who returns lines (NOT including newline character(s)).
   *  It will treat any of \r\n, \r, or \n as a line separator (longest match) - if
   *  you need more refined behavior you can subclass Source#LineIterator directly.
   */
  def getLines(): Iterator[String] = new LineIterator()

  /** Returns `**true**` if this source has more characters. */
  def hasNext: Boolean = iter.hasNext

  /** Returns next character. */
  def next(): Char = positioner.next()

  @nowarn("cat=deprecation")
  /** Tracks the line and column position of each character as it is read from this source.
   *
   *  @param encoder the `Position` used to encode line and column numbers into a single position value
   */
  class Positioner(encoder: Position) {
    /** Creates a `Positioner` that encodes positions using `RelaxedPosition`.
     */
    def this() = this(RelaxedPosition)
    /** the last character returned by next. */
    var ch: Char = compiletime.uninitialized

    /** Position of last character returned by next. */
    var pos = 0

    /** Current line and column. */
    var cline = 1
    var ccol = 1

    /** Default col increment for tabs '\t', set to 4 initially. */
    var tabinc = 4

    /** Returns the next character from the underlying iterator, updating the
     *  current character, encoded position, line, and column.
     */
    def next(): Char = {
      ch = iter.next()
      pos = encoder.encode(cline, ccol)
      ch match {
        case '\n' =>
          ccol = 1
          cline += 1
        case '\t' =>
          ccol += tabinc
        case _ =>
          ccol += 1
      }
      ch
    }
  }
  /** A Position implementation which ignores errors in
   *  the positions.
   */
  @nowarn("cat=deprecation")
  object RelaxedPosition extends Position {
    /** Performs no validation, accepting any line and column without throwing.
     *
     *  @param line the line number that would otherwise be validated
     *  @param column the column number that would otherwise be validated
     */
    def checkInput(line: Int, column: Int): Unit = ()
  }
  object RelaxedPositioner extends Positioner(RelaxedPosition) { }
  object NoPositioner extends Positioner(Position) {
    /** Returns the next character from the underlying iterator without tracking
     *  its position.
     */
    override def next(): Char = iter.next()
  }
  /** Returns the character recorded by the current positioner, which for a
   *  position-tracking positioner is the last character returned by `next`.
   */
  def ch: Char = positioner.ch
  /** Returns the encoded position recorded by the current positioner, which for a
   *  position-tracking positioner is the position of the last character returned by `next`.
   */
  def pos: Int = positioner.pos

  /** Reports an error message to the output stream `out`.
   *
   *  @param pos the source position (line/column)
   *  @param msg the error message to report
   *  @param out PrintStream to use (optional: defaults to `Console.err`)
   */
  def reportError(
    pos: Int,
    msg: String,
    out: PrintStream = Console.err): Unit =
  {
    nerrors += 1
    report(pos, msg, out)
  }

  private def spaces(n: Int) = " ".repeat(n)
  /**
   *  @param pos the source position (line/column)
   *  @param msg the error message to report
   *  @param out PrintStream to use
   */
  def report(pos: Int, msg: String, out: PrintStream): Unit = {
    val line  = Position line pos
    val col   = Position column pos

    out.println("%s:%d:%d: %s%s%s^".format(descr, line, col, msg, lineNum(line), spaces(col - 1)))
  }

  /**
   *  @param pos the source position (line/column)
   *  @param msg the warning message to report
   *  @param out PrintStream to use (optional: defaults to `Console.out`)
   */
  def reportWarning(
    pos: Int,
    msg: String,
    out: PrintStream = Console.out): Unit =
  {
    nwarnings += 1
    report(pos, "warning! " + msg, out)
  }

  @annotation.stableNull
  private var resetFunction: (() => Source) | Null = null
  @annotation.stableNull
  private var closeFunction: (() => Unit) | Null = null
  private var positioner: Positioner = RelaxedPositioner

  /** Sets the function that [[reset]] uses to produce a fresh copy of this source.
   *
   *  @param f a `() => Source` that produces a fresh copy of this source, or `null` to leave [[reset]] throwing
   *  @return this source
   */
  def withReset(f: (() => Source) | Null): this.type = {
    resetFunction = f
    this
  }
  /** Sets the function that [[close]] uses to close the underlying resource.
   *
   *  @param f a `() => Unit` that closes the underlying resource, or `null` to make [[close]] a no-op
   *  @return this source
   */
  def withClose(f: (() => Unit) | Null): this.type = {
    closeFunction = f
    this
  }
  /** Sets the description of this source.
   *
   *  @param text the description to use
   *  @return this source
   */
  def withDescription(text: String): this.type = {
    descr = text
    this
  }
  /** Change or disable the positioner.
   *
   *  @param on whether to enable (`true`) or disable (`false`) position tracking
   */
  def withPositioning(on: Boolean): this.type = {
    positioner = if (on) RelaxedPositioner else NoPositioner
    this
  }
  /** Sets a custom positioner used to track line and column positions.
   *
   *  @param pos the positioner to use
   *  @return this source
   */
  def withPositioning(pos: Positioner): this.type = {
    positioner = pos
    this
  }

  /** The close() method closes the underlying resource. */
  def close(): Unit = {
    if (closeFunction != null) closeFunction()
  }

  /** The reset() method creates a fresh copy of this Source. */
  def reset(): Source =
    if (resetFunction != null) resetFunction()
    else throw new UnsupportedOperationException("Source's reset() method was not set.")
}
