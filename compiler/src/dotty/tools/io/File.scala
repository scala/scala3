/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package dotty.tools.io

import java.io.{File => JavaIoFile, _}
import java.nio.file.{Files, Paths}
import java.nio.file.StandardOpenOption.*

import scala.io.Codec
/**
 * ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
object File {
  def pathSeparator: String = JavaIoFile.pathSeparator
  def separator: String     = JavaIoFile.separator

  def apply(path: String)(implicit codec: Codec): File = apply(Paths.get(path))
  def apply(path: JPath)(implicit codec: Codec): File = new File(path)
}

/** An abstraction for files.  For character data, a Codec
 *  can be supplied at either creation time or when a method
 *  involving character data is called (with the latter taking
 *  precedence if supplied.) If neither is available, the value
 *  of scala.io.Codec.default is used.
 *
 *  @author  Paul Phillips
 *  @since   2.8
 *
 *  ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
class File(jpath: JPath)(implicit constructorCodec: Codec) extends Path(jpath) {
  override def toAbsolute: File = if (isAbsolute) this else super.toAbsolute.toFile
  override def toDirectory: Directory = new Directory(jpath)
  override def toFile: File = this
  override def normalize: File = super.normalize.toFile
  override def length: Long = super[Path].length
  override def walkFilter(cond: Path => Boolean): Iterator[Path] =
    if (cond(this)) Iterator.single(this) else Iterator.empty

  /** Obtains an InputStream. */
  def inputStream(): InputStream = Files.newInputStream(jpath)

  /** Obtains a OutputStream. */
  def outputStream(append: Boolean = false): OutputStream =
    if (append) Files.newOutputStream(jpath, CREATE, APPEND)
    else Files.newOutputStream(jpath, CREATE, TRUNCATE_EXISTING)

  /** Obtains an OutputStreamWriter wrapped around a FileOutputStream.
   *  This should behave like a less broken version of java.io.FileWriter,
   *  in that unlike the java version you can specify the encoding.
   */
  private def writer(append: Boolean, codec: Codec): OutputStreamWriter =
    new OutputStreamWriter(outputStream(append), codec.charSet)

  /** Wraps a BufferedWriter around the result of writer().
   */
  def bufferedWriter(): BufferedWriter = bufferedWriter(append = false)
  def bufferedWriter(append: Boolean): BufferedWriter = bufferedWriter(append, constructorCodec)
  def bufferedWriter(append: Boolean, codec: Codec): BufferedWriter =
    new BufferedWriter(writer(append, codec))

  def printWriter(): PrintWriter = new PrintWriter(bufferedWriter(), true)

  /** Creates a new file and writes all the Strings to it. */
  def writeAll(strings: String*): Unit = {
    val out = bufferedWriter()
    try strings.foreach(out.write(_))
    finally out.close()
  }
}
