/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package dotty.tools
package io

import java.io.{InputStream, OutputStream}
import java.net.URL
import java.nio.file.{InvalidPathException, Paths}
import scala.io.Codec

/** ''Note:  This library is considered experimental and should not be used unless you know what you are doing.'' */
class PlainDirectory(givenPath: Directory) extends PlainFile(givenPath) {
  override val isDirectory: Boolean = true
  override def iterator: Iterator[PlainFile] = givenPath.list.filter(_.exists).map(new PlainFile(_))
}

/** This class implements an abstract file backed by a File.
 *
 * ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
class PlainFile(givenPath: Path) extends AbstractFile {
  dotc.util.Stats.record("new PlainFile")

  override def jpath: JPath = givenPath.jpath

  override def name: String = givenPath.name

  // Interned for fast hashcode and equals
  override val path: String = givenPath.normalize.path.intern

  override def container: Option[AbstractFile] = Some(new PlainFile(givenPath.parent))
  override def input: InputStream = givenPath.toFile.inputStream()
  override def readAsString(codec: Codec): String = java.nio.file.Files.readString(givenPath.jpath, codec.charSet)
  override def output: OutputStream = givenPath.toFile.outputStream()
  override def toURL: Option[URL] = Some(jpath.toUri.toURL)

  override def hashCode(): Int = System.identityHashCode(path)
  override def equals(that: Any): Boolean = that match {
    case x: PlainFile => path `eq` x.path
    case _            => false
  }

  /** Is this abstract file a directory? */
  override val isDirectory: Boolean = givenPath.isDirectory // cached for performance on Windows

  /** Returns the time that this abstract file was last modified. */
  override def lastModified: Long = givenPath.lastModified.toMillis

  /** Returns all abstract subfiles of this abstract directory. */
  override def iterator: Iterator[AbstractFile] = {
    // Optimization: Assume that the file was not deleted and did not have permissions changed
    // between the call to `list` and the iteration. This saves a call to `exists`.
    def existsFast(path: Path) = path match {
      case (_: Directory | _: File) => true
      case _ => path.exists
    }
    givenPath.toDirectory.list.filter(existsFast).map(new PlainFile(_))
  }

  /**
   * Returns the abstract file in this abstract directory with the
   * specified name. If there is no such file, returns null. The
   * argument "directory" tells whether to look for a directory or
   * or a regular file.
   */
  override def lookupName(name: String, directory: Boolean): AbstractFile | Null = {
    val child = givenPath / name
    if directory then
      if child.isDirectory /* IO! */ then
        new PlainFile(child)
      else
        null
    else if child.isFile /* IO! */ then
      new PlainFile(child)
    else
      null
  }

  // preserve whatever we got as input
  override def toString(): String = givenPath.toString()
}

object PlainFile {
  extension (jPath: JPath)
    def toPlainFile = new PlainFile(new Path(jPath))
}
