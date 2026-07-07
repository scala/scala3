/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package dotty.tools.io

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}
import java.net.{URI, URL}

/** This class implements an in-memory file.
 *
 *  @author  Philippe Altherr
 *  @version 1.0, 23/03/2004
 *
 *  ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
class VirtualFile(override val path: String, initialContents: Array[Byte]) extends AbstractFile {
  private var content = initialContents

  override val name: String = {
    // We support fake names like "<example>" even on Windows where Path.of would throw.
    // TODO: Proper path support for VirtualFile, should be integrated with VirtualDirectory...
    if path.startsWith("<") then path
    else
      val fileName = java.nio.file.Path.of(path).getFileName
      if fileName == null then ""
      else fileName.toString
  }

  // For compatibility, until we remove `AbstractFile.jpath`.
  override def jpath: JPath | Null = try java.nio.file.Path.of(path) catch case _: Exception => null

  override def sizeOption: Option[Int] = Some(content.length)

  override def toURL: Option[URL] = None

  /** Always returns true, even if jpath is a non-existing file. */
  override def exists: Boolean = true

  override def input: InputStream = new ByteArrayInputStream(content)

  override def output(append: Boolean = false): OutputStream =
    new ByteArrayOutputStream() {
      override def close(): Unit = {
        super.close()
        val written = toByteArray
        content = if append then content ++ written else written
      }
    }

  /** Is this abstract file a directory? */
  override def isDirectory: Boolean = false

  /** @inheritdoc */
  override def isVirtual: Boolean = true

  override def lastModified: Long = 0

  override def iterator: Iterator[AbstractFile] = unsupported()

  override def lookupName(name: String, directory: Boolean): AbstractFile | Null = unsupported()
}
