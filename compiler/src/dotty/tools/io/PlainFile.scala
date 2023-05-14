/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package dotty.tools
package io

import scala.language.unsafeNulls

import java.io.{InputStream, OutputStream}
import java.nio.file.{InvalidPathException, Paths}

/** ''Note:  This library is considered experimental and should not be used unless you know what you are doing.'' */
class PlainDirectory(givenPath: Directory) extends PlainFile(givenPath) {
  override def isDirectory: Boolean = true
  override def iterator(): Iterator[PlainFile] = givenPath.list.filter(_.exists).map(new PlainFile(_))
  override def delete(): Unit = givenPath.deleteRecursively()
}

/** This class implements an abstract file backed by a File.
 *
 * ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
class PlainFile(val givenPath: Path) extends AbstractFile {
  assert(path ne null)

  dotc.util.Stats.record("new PlainFile")

  def jpath: JPath = givenPath.jpath

  override def underlyingSource  = {
    val fileSystem = jpath.getFileSystem
    fileSystem.provider().getScheme match {
      case "jar" =>
        val fileStores = fileSystem.getFileStores.iterator()
        if (fileStores.hasNext) {
          val jarPath = fileStores.next().name
          try {
            Some(new PlainFile(new Path(Paths.get(jarPath.stripSuffix(fileSystem.getSeparator)))))
          } catch {
            case _: InvalidPathException =>
              None
          }
        } else None
      case "jrt" =>
        if (jpath.getNameCount > 2 && jpath.startsWith("/modules")) {
          // TODO limit this to OpenJDK based JVMs?
          val moduleName = jpath.getName(1)
          Some(new PlainFile(new Path(Paths.get(System.getProperty("java.home"), "jmods", moduleName.toString + ".jmod"))))
        } else None
      case _ => None
    }
  }


  /** Returns the name of this abstract file. */
  def name: String = givenPath.name

  /** Returns the path of this abstract file. */
  def path: String = givenPath.path

  /** Returns the absolute path of this abstract file as an interned string. */
  override val absolutePath: String = givenPath.toAbsolute.toString.intern

  /** The absolute file. */
  def absolute: PlainFile = new PlainFile(givenPath.toAbsolute)

  override def container: AbstractFile = new PlainFile(givenPath.parent)
  override def input: InputStream = givenPath.toFile.inputStream()
  override def output: OutputStream = givenPath.toFile.outputStream()
  override def sizeOption: Option[Int] = Some(givenPath.length.toInt)

  override def hashCode(): Int = System.identityHashCode(absolutePath)
  override def equals(that: Any): Boolean = that match {
    case x: PlainFile => absolutePath `eq` x.absolutePath
    case _            => false
  }

  /** Is this abstract file a directory? */
  def isDirectory: Boolean = givenPath.isDirectory

  /** Returns the time that this abstract file was last modified. */
  def lastModified: Long = givenPath.lastModified.toMillis

  /** Returns all abstract subfiles of this abstract directory. */
  def iterator: Iterator[AbstractFile] = {
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
  def lookupName(name: String, directory: Boolean): AbstractFile = {
    val child = givenPath / name
    if ((child.isDirectory && directory) || (child.isFile && !directory)) new PlainFile(child)
    else null
  }

  /** Does this abstract file denote an existing file? */
  def create(): Unit = if (!exists) givenPath.createFile()

  /** Delete the underlying file or directory (recursively). */
  def delete(): Unit =
    if (givenPath.isFile) givenPath.delete()
    else if (givenPath.isDirectory) givenPath.toDirectory.deleteRecursively()

  /** Returns a plain file with the given name. It does not
   *  check that it exists.
   */
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile =
    new PlainFile(givenPath / name)
}

object PlainFile {
  extension (jPath: JPath)
    def toPlainFile = new PlainFile(new Path(jPath))
}
