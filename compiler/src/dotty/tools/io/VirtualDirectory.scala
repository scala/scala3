/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 */

package dotty.tools.io

import scala.collection.mutable
import java.io.{InputStream, OutputStream}
import java.net.URL
/**
 * An in-memory directory.
 *
 * @author Lex Spoon
 *
 * ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
class VirtualDirectory private[io] (val name: String, maybeContainer: Option[VirtualDirectory] = None) extends AbstractFile {
  override def path: String =
    maybeContainer match {
      case None => name
      case Some(parent) => parent.path + '/' + name
    }

  override def container: Option[VirtualDirectory] = maybeContainer
  override def isDirectory: Boolean = true
  override def isVirtual: Boolean = true
  override val lastModified: Long = System.currentTimeMillis

  override def jpath: JPath | Null = null
  override def toURL: Option[URL] = None
  override def input: InputStream = sys.error("directories cannot be read")
  override def output(append: Boolean = false): OutputStream = sys.error("directories cannot be written")

  private val files = mutable.Map.empty[String, AbstractFile]

  // the toList is so that the directory may continue to be
  // modified while its elements are iterated
  override def iterator: Iterator[AbstractFile] = files.values.toList.iterator

  override def lookupName(name: String, directory: Boolean): AbstractFile | Null =
    (files get name filter (_.isDirectory == directory)).orNull

  override def fileNamed(name: String): AbstractFile =
    Option(lookupName(name, directory = false)) getOrElse {
      val newFile = new VirtualFile(s"$path/$name", Array.emptyByteArray)
      files(name) = newFile
      newFile
    }

  override def subdirectoryNamed(name: String): AbstractFile =
    Option(lookupName(name, directory = true)) getOrElse {
      val dir = new VirtualDirectory(name, Some(this))
      files(name) = dir
      dir
    }
}
