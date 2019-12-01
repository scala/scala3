package dotty.tools.tasty.experimental

import collection.mutable

import dotty.tools.tasty._

import TastyBuffer._
import scala.io.Codec

class NameBuffer[T <: Tasty](given val tasty: T) extends TastyBuffer(10000) {
  import tasty.{_, given}
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
            nameIndex(original)
            nameIndex(result)
            params.foreach {
              case param: TypeName =>
                nameIndex(param)
              case _ =>
            }
          case AnyQualifiedName(prefix, name) =>
            nameIndex(prefix); nameIndex(name)
          case AnyUniqueName(original, separator, num) =>
            nameIndex(separator.toTermName)
            if (!original.isEmpty) nameIndex(original)
          case DerivedName(original) =>
            nameIndex(original)
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
    val length = currentAddr.index - lengthAddr.index - lengthWidth
    putNat(lengthAddr, length, lengthWidth)
  }

  def writeNameRef(ref: NameRef): Unit = writeNat(ref.index)
  def writeNameRef(name: Name): Unit = writeNameRef(nameRefs(name.toTermName))

  def writeParamSig(paramSig: Signature.ParamSig): Unit = {
    val encodedValue = paramSig.foldInt(
      paramSig => -paramSig,
      paramSig => nameRefs(paramSig.toTermName).index
    )
    writeInt(encodedValue)
  }

  def pickleNameContents(name: Name): Unit = {
    val tag = name.toTermName.tag
    writeByte(tag)
    name.toTermName match {
      case name: SimpleName =>
        val bytes = name.toUTF8
        writeNat(bytes.length)
        writeBytes(bytes, bytes.length)
      case AnyQualifiedName(prefix, name) =>
        withLength { writeNameRef(prefix); writeNameRef(name) }
      case AnyUniqueName(original, separator, num) =>
        withLength {
          writeNameRef(separator.toTermName)
          writeNat(num)
          if (!original.isEmpty) writeNameRef(original)
        }
      case AnyNumberedName(original, num) =>
        withLength { writeNameRef(original); writeNat(num) }
      case SignedName(original, Signature(paramsSig, result)) =>
        withLength(
          { writeNameRef(original); writeNameRef(result); paramsSig.foreach(writeParamSig) },
          if ((paramsSig.length + 2) * maxIndexWidth <= maxNumInByte) 1 else 2)
      case DerivedName(original) =>
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
