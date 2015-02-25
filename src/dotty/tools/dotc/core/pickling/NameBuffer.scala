package dotty.tools
package dotc
package core
package pickling

import collection.mutable
import Names.{Name, chrs}
import Decorators._
import TastyBuffer._
import scala.io.Codec
import TastyName._
import PickleFormat._

class NameBuffer extends TastyBuffer(100000) {

  private val nameRefs = new mutable.LinkedHashMap[TastyName, NameRef]

  def nameIndex(name: TastyName): NameRef = nameRefs.get(name) match {
    case Some(ref) =>
      ref
    case None =>
      val ref = NameRef(nameRefs.size)
      nameRefs(name) = ref
      ref
  }
  def nameIndex(name: Name): NameRef = nameIndex(Simple(name.toTermName))
  def nameIndex(str: String): NameRef = nameIndex(str.toTermName)
  
  private def withLength(op: => Unit): Unit = {
    val lengthAddr = currentAddr
    writeByte(0)
    op
    val length = currentAddr.index - lengthAddr.index - 1
    assert(length < 128)
    putNat(lengthAddr, length, 1)
  }
  
  def writeNameRef(ref: NameRef) = writeNat(ref.index)
  
  def pickleName(name: TastyName): Unit = name match {
    case Simple(name) => 
      val bytes = 
        if (name.length == 0) new Array[Byte](0)
        else Codec.toUTF8(chrs, name.start, name.length)
      writeByte(UTF8)
      writeNat(bytes.length)
      writeBytes(bytes, bytes.length)
    case Qualified(qualified, selector) =>
      writeByte(QUALIFIED)
      withLength { writeNameRef(qualified); writeNameRef(selector) }
    case Signed(original, params, result) => 
      writeByte(SIGNED)
      withLength { writeNameRef(original); writeNameRef(result); params.foreach(writeNameRef) }
    case Expanded(original) =>
      writeByte(EXPANDED)
      withLength { writeNameRef(original) }
    case ModuleClass(module) =>
      writeByte(MODULECLASS)
      withLength { writeNameRef(module) }
    case SuperAccessor(accessed) =>
      writeByte(SUPERACCESSOR)
      withLength { writeNameRef(accessed) }
    case DefaultGetter(method, paramNumber) =>
      writeByte(DEFAULTGETTER)
      withLength { writeNameRef(method); writeNat(paramNumber) }
  }
  
  override def assemble(): Unit = {
    var i = 0
    for ((name, ref) <- nameRefs) {
      assert(ref.index == i)
      i += 1
      pickleName(name)
    }
  }
}
