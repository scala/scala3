/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package dotty.tools.io

import java.io.InputStream
import java.net.URL

/** A distinguished object so you can avoid both null
 *  and Option.
 *
 *  ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
object NoAbstractFile extends AbstractFile {
  override def jpath: JPath | Null = null
  override def input: InputStream = throw UnsupportedOperationException("NoAbstractFile.input")
  override def isDirectory: Boolean = false
  override def isVirtual: Boolean = true
  override def iterator: Iterator[AbstractFile] = Iterator.empty
  override def lastModified: Long = 0L
  override def lookupName(name: String, directory: Boolean): AbstractFile | Null = null
  override def name: String = ""
  override def output: java.io.OutputStream = throw UnsupportedOperationException("NoAbstractFile.output")
  override def path: String = ""
  override def toURL: Option[URL] = None
  override def toString(): String = "<no file>"
}
