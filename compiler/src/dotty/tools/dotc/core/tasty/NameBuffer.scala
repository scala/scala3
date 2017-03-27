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

  def pickleNameContents(name: Name): Unit = name.toTermName match {
    case name: SimpleTermName =>
      val bytes =
        if (name.length == 0) new Array[Byte](0)
        else Codec.toUTF8(chrs, name.start, name.length)
      writeByte(UTF8)
      writeNat(bytes.length)
      writeBytes(bytes, bytes.length)
    case QualifiedName(qualified, selector) =>
      writeByte(QUALIFIED)
      withLength { writeNameRef(qualified); writeNameRef(selector) }
    case FlattenedName(qualified, selector) =>
      writeByte(FLATTENED)
      withLength { writeNameRef(qualified); writeNameRef(selector) }
    case XpandedName(prefix, original) =>
      writeByte(EXPANDED)
      withLength { writeNameRef(prefix); writeNameRef(original) }
    case SignedName(original, Signature(params, result)) =>
      writeByte(SIGNED)
      withLength(
          { writeNameRef(original); writeNameRef(result); params.foreach(writeNameRef) },
          if ((params.length + 2) * maxIndexWidth <= maxNumInByte) 1 else 2)
    case ModuleClassName(module) =>
      writeByte(OBJECTCLASS)
      withLength { writeNameRef(module) }
    case SuperAccessorName(accessed) =>
      writeByte(SUPERACCESSOR)
      withLength { writeNameRef(accessed) }
    case DefaultGetterName(method, paramNumber) =>
      writeByte(DEFAULTGETTER)
      withLength { writeNameRef(method); writeNat(paramNumber) }
    case ShadowedName(original) =>
      writeByte(SHADOWED)
      withLength { writeNameRef(original) }
    case VariantName(original, sign) =>
      writeByte(VARIANT)
      withLength { writeNameRef(original); writeNat(sign + 1) }
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
