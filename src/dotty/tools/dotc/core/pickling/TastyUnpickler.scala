package dotty.tools.dotc
package core
package pickling

import scala.collection.mutable
import PickleFormat._
import Names.{Name, termName}

object TastyUnpickler {  
  class UnpickleException(msg: String) extends Exception(msg)
  
  abstract class SectionUnpickler[R](val name: String) {
    def unpickle(reader: TastyReader, tastyName: TastyName.Table): R
  }
}

import TastyUnpickler._

class TastyUnpickler(reader: TastyReader) {
  import reader._
    
  def this(bytes: Array[Byte]) = this(new TastyReader(bytes))
  
  private val sectionReader = new mutable.HashMap[String, TastyReader]
  val tastyName = new TastyName.Table
  
  def check(cond: Boolean, msg: => String) = 
    if (!cond) throw new UnpickleException(msg)
  
  def readString(): String = {
    val TastyName.Simple(name) = tastyName(readNameRef())
    name.toString
  }
    
  def readName(): TastyName = {
    import TastyName._
    val tag = readByte() 
    val length = readNat()
    val start = currentAddr
    val end = start + length
    val result = tag match {
      case UTF8 => 
        skipTo(end)
        Simple(termName(bytes, start.index, length))
      case QUALIFIED =>
        Qualified(readNameRef(), readNameRef())
      case SIGNED =>
        val original = readNameRef()
        val result = readNameRef()
        val params = until(end)(readNameRef())
        Signed(original, params, result)
      case EXPANDED =>
        Expanded(readNameRef())
      case MODULECLASS =>
        ModuleClass(readNameRef())
      case SUPERACCESSOR =>
        SuperAccessor(readNameRef())
      case DEFAULTGETTER =>
        DefaultGetter(readNameRef(), readNat())
    }
    assert(currentAddr == end, s"bad name $result $start $currentAddr $end")
    result
  }
  
  locally {
    val magic = readBytes(8)
    check(magic.map(_.toChar).mkString == header, "not a TASTy file")
    val major = readNat()
    val minor = readNat()
    def versionMsg = 
      s"""TASTy signature has wrong version.
         | expected: $MajorVersion.$MinorVersion
         | found   : $major.$minor""".stripMargin
    check(major == MajorVersion, versionMsg)
    if (MajorVersion == 0) check(minor == MinorVersion, versionMsg)
    until(readEnd()) { tastyName.add(readName()) }
    while (!isAtEnd) {
      val secName = readString()
      val secEnd = readEnd()
      sectionReader(secName) = new TastyReader(bytes, currentAddr.index, secEnd.index, currentAddr.index)
      skipTo(secEnd)
    }
  }
  
  def unpickled[R](sec: SectionUnpickler[R]): Option[R] = 
    for (reader <- sectionReader.get(sec.name)) yield
      sec.unpickle(reader, tastyName)
}
