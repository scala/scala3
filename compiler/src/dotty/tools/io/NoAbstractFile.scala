/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package dotty.tools.io

import java.io.InputStream

/** A distinguished object so you can avoid both null
 *  and Option.
 *
 *  ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
object NoAbstractFile extends AbstractFile {
  def absolute: AbstractFile = this
  def container: AbstractFile = this
  def jpath: JPath | Null = null
  def input: InputStream = throw UnsupportedOperationException("NoAbstractFile.input")
  def isDirectory: Boolean = false
  override def isVirtual: Boolean = true
  def iterator: Iterator[AbstractFile] = Iterator.empty
  def lastModified: Long = 0L
  def lookupName(name: String, directory: Boolean): AbstractFile | Null = null

  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile =
    throw UnsupportedOperationException("NoAbstractFile.lookupNameUnchecked")

  def name: String = ""
  def output: java.io.OutputStream = throw UnsupportedOperationException("NoAbstractFile.output")
  def path: String = ""
  override def toByteArray: Array[Byte] = Array[Byte]()
  override def toString: String = "<no file>"
}
