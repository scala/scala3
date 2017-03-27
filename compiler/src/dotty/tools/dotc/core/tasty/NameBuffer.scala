package dotty.tools
package dotc
package core
package tasty

import collection.mutable
import Names.{Name, chrs, SimpleTermName, DerivedTermName}
import NameOps.NameDecorator
import NameExtractors._
import Decorators._
import TastyBuffer._
import scala.io.Codec
import TastyFormat._

class NameBuffer extends TastyBuffer(10000) {
  import NameBuffer._

  private val nameRefs = new mutable.LinkedHashMap[Name, NameRef]

  def nameIndex(name: Name): NameRef = {
    val name1 = name.toTermName
    nameRefs.get(name1) match {
      case Some(ref) =>
        ref
      case None =>
        name1 match {
          case SignedName(original, Signature(params, result)) =>
            nameIndex(original); nameIndex(result); params.foreach(nameIndex)
          case AnyQualifiedName(prefix, info) =>
            nameIndex(prefix); nameIndex(info.name)
          case DerivedTermName(prefix, _) =>
            nameIndex(prefix)
          case _ =>
        }
        val ref = NameRef(nameRefs.size)
        nameRefs(name1) = ref
        ref
      }
  }

  private def withLength(op: => Unit, lengthWidth: Int = 1): Unit = {
    val lengthAddr = currentAddr
    for (i <- 0 until lengthWidth) writeByte(0)
    op
    val length = currentAddr.index - lengthAddr.index - 1
    putNat(lengthAddr, length, lengthWidth)
  }

  def writeNameRef(ref: NameRef): Unit = writeNat(ref.index)
  def writeNameRef(name: Name): Unit = writeNameRef(nameRefs(name.toTermName))

  def pickleNameContents(name: Name): Unit = {
    writeByte(name.toTermName.info.tag)
    name.toTermName match {
      case name: SimpleTermName =>
        val bytes =
          if (name.length == 0) new Array[Byte](0)
          else Codec.toUTF8(chrs, name.start, name.length)
        writeNat(bytes.length)
        writeBytes(bytes, bytes.length)
      case AnyQualifiedName(prefix, info) =>
        withLength { writeNameRef(prefix); writeNameRef(info.name) }
      case SignedName(original, Signature(params, result)) =>
        withLength(
          { writeNameRef(original); writeNameRef(result); params.foreach(writeNameRef) },
          if ((params.length + 2) * maxIndexWidth <= maxNumInByte) 1 else 2)
      case DefaultGetterName(method, paramNumber) =>
        withLength { writeNameRef(method); writeNat(paramNumber) }
      case VariantName(original, sign) =>
        withLength { writeNameRef(original); writeNat(sign + 1) }
      case DerivedTermName(original, info) =>
        withLength { writeNameRef(original) }
    }
  }

  override def assemble(): Unit = {
    var i = 0
    for ((name, ref) <- nameRefs) {
      assert(ref.index == i)
      i += 1
      pickleNameContents(name)
    }
  }
}

object NameBuffer {
  private val maxIndexWidth = 3  // allows name indices up to 2^21.
  private val payloadBitsPerByte = 7 // determined by nat encoding in TastyBuffer
  private val maxNumInByte = (1 << payloadBitsPerByte) - 1
}
