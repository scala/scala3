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

  private val nameRefs = new mutable.LinkedHashMap[TastyName, Ref]

  def nameIndex(name: TastyName): Ref = nameRefs.get(name) match {
    case Some(ref) =>
      ref
    case None =>
      val ref = new Ref(nameRefs.size)
      nameRefs(name) = ref
      ref
  }
  def nameIndex(name: Name): Ref = nameIndex(Simple(name.toTermName))
  def nameIndex(str: String): Ref = nameIndex(str.toTermName)
  
  private def withLength(op: => Unit): Unit = {
    val lengthAddr = currentAddr
    writeByte(0)
    op
    val length = currentAddr.index - lengthAddr.index - 1
    assert(length < 128)
    putNat(lengthAddr, length, 1)
  }
  
  def writeRef(ref: Ref) = writeNat(ref.index)
  
  def pickleName(name: TastyName): Unit = name match {
    case Simple(name) => 
      val bytes = Codec.toUTF8(chrs, name.start, name.length)
      writeByte(UTF8)
      writeNat(bytes.length)
      writeBytes(bytes, bytes.length)
    case Qualified(qualified, selector) =>
      writeByte(QUALIFIED)
      withLength { writeRef(qualified); writeRef(selector) }
    case Signed(original, params, result) => 
      writeByte(SIGNED)
      withLength { writeRef(original); writeRef(result); params.foreach(writeRef) }
    case Expanded(original) =>
      writeByte(EXPANDED)
      withLength { writeRef(original) }
    case ModuleClass(module) =>
      writeByte(MODULECLASS)
      withLength { writeRef(module) }
    case SuperAccessor(accessed) =>
      writeByte(SUPERACCESSOR)
      withLength { writeRef(accessed) }
    case DefaultGetter(method, paramNumer) =>
      writeByte(DEFAULTGETTER)
      withLength { writeRef(method); writeNat(paramNumer) }
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
