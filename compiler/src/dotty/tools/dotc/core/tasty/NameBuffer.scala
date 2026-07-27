package dotty.tools
package dotc
package core
package tasty

import dotty.tools.tasty.TastyBuffer
import TastyBuffer.*

import collection.mutable
import Names.{Name, chrs, SimpleName, DerivedName, TypeName}
import NameKinds.*
import NameOps.*
import scala.io.Codec
import NameTags.{SIGNED, TARGETSIGNED}

class NameBuffer extends TastyBuffer(10000) {
  import NameBuffer.*

  private val nameRefs = new mutable.LinkedHashMap[Name, NameRef]
  private val stringLiteralRefs = new mutable.HashMap[String, NameRef]
  private val utf8NameRefs = new mutable.HashMap[String, NameRef]
  private val nameEntries = new mutable.ArrayBuffer[Name | String]

  private def addEntry(entry: Name | String): NameRef = {
    val ref = NameRef(nameEntries.size)
    nameEntries += entry
    ref
  }

  def nameIndex(name: Name): NameRef = {
    val name1 = name.toTermName
    nameRefs.get(name1) match {
      case Some(ref) =>
        ref
      case None =>
        name1 match {
          case SignedName(original, Signature(params, result), target) =>
            nameIndex(original)
            if !original.matchesTargetName(target) then nameIndex(target)
            nameIndex(result)
            params.foreach {
              case param: TypeName =>
                nameIndex(param)
              case _ =>
            }
          case AnyQualifiedName(prefix, name) =>
            nameIndex(prefix); nameIndex(name)
          case AnyUniqueName(original, separator, num) =>
            nameIndex(separator)
            if (!original.isEmpty) nameIndex(original)
          case DerivedName(original, _) =>
            nameIndex(original)
          case _ =>
        }
        val ref = name1 match
          case name: SimpleName =>
            val value = simpleNameValue(name)
            stringLiteralRefs.get(value) match
              case Some(ref) => ref
              case None =>
                val ref = addEntry(name1)
                utf8NameRefs(value) = ref
                ref
          case _ =>
            addEntry(name1)
        nameRefs(name1) = ref
        ref
    }
  }

  /** Reserve a UTF8 name-table entry for a raw string, keyed by the string
   *  itself rather than by an interned `Name`. Source paths (`utf8Index`,
   *  `PositionPickler.pickleSource`) and TASTy string constants all funnel
   *  through here so that, e.g., the `@SourceFile` annotation argument, the
   *  position source path, and the `SOURCEFILEattr` value still collapse to a
   *  single entry without allocating a global `TermName`.
   */
  def stringLiteralIndex(value: String): NameRef =
    stringLiteralRefs.get(value) match
      case Some(ref) => ref
      case None =>
        val ref = utf8NameRefs.getOrElse(value, {
          val ref = addEntry(value)
          utf8NameRefs(value) = ref
          ref
        })
        stringLiteralRefs(value) = ref
        ref

  def utf8Index(value: String): NameRef = stringLiteralIndex(value)

  private inline def withLength(inline op: Unit, lengthWidth: Int = 1): Unit = {
    val lengthAddr = currentAddr
    var i = 0
    while i < lengthWidth do
      writeByte(0)
      i += 1
    op
    val length = currentAddr.index - lengthAddr.index - lengthWidth
    putNat(lengthAddr, length, lengthWidth)
  }

  def writeNameRef(ref: NameRef): Unit = writeNat(ref.index)
  def writeNameRef(name: Name): Unit = writeNameRef(nameRefs(name.toTermName))

  def writeParamSig(paramSig: Signature.ParamSig): Unit ={
    val encodedValue = paramSig match {
      case paramSig: TypeName =>
        nameRefs(paramSig.toTermName).index
      case paramSig: Int =>
        -paramSig
    }
    writeInt(encodedValue)
  }

  private def pickleUtf8String(value: String): Unit = {
    writeByte(NameTags.UTF8)
    val bytes =
      if value.isEmpty then new Array[Byte](0)
      else Codec.toUTF8(value)
    writeNat(bytes.length)
    writeBytes(bytes, bytes.length)
  }

  def pickleNameContents(name: Name): Unit = {
    val tag = name.toTermName.info.kind.tag
    name.toTermName match {
      case name: SimpleName =>
        writeByte(tag)
        val bytes =
          if (name.length == 0) new Array[Byte](0)
          else Codec.toUTF8(chrs, name.start, name.length)
        writeNat(bytes.length)
        writeBytes(bytes, bytes.length)
      case AnyQualifiedName(prefix, name) =>
        writeByte(tag)
        withLength { writeNameRef(prefix); writeNameRef(name) }
      case AnyUniqueName(original, separator, num) =>
        writeByte(tag)
        withLength {
          writeNameRef(separator)
          writeNat(num)
          if (!original.isEmpty) writeNameRef(original)
        }
      case AnyNumberedName(original, num) =>
        writeByte(tag)
        withLength { writeNameRef(original); writeNat(num) }
      case SignedName(original, Signature(paramsSig, result), target) =>
        val needsTarget = !original.matchesTargetName(target)
        writeByte(if needsTarget then TARGETSIGNED else SIGNED)
        withLength(
          { writeNameRef(original)
            if needsTarget then writeNameRef(target)
            writeNameRef(result)
            paramsSig.foreach(writeParamSig)
          },
          if ((paramsSig.length + 3) * maxIndexWidth <= maxNumInByte) 1 else 2)
      case DerivedName(original, _) =>
        writeByte(tag)
        withLength { writeNameRef(original) }
    }
  }

  override def assemble(): Unit = {
    var i = 0
    for entry <- nameEntries do
      entry match
        case name: Name =>
          assert(nameRefs(name).index == i)
          pickleNameContents(name)
        case value: String =>
          assert(stringLiteralRefs(value).index == i)
          pickleUtf8String(value)
      i += 1
    assert(i == nameEntries.size)
    val refs = new mutable.HashSet[Int]
    refs.sizeHint(nameRefs.size + stringLiteralRefs.size)
    nameRefs.valuesIterator.foreach(ref => refs += ref.index)
    stringLiteralRefs.valuesIterator.foreach(ref => refs += ref.index)
    assert(refs.size == nameEntries.size)
  }
}

object NameBuffer {
  private val maxIndexWidth = 3  // allows name indices up to 2^21.
  private val payloadBitsPerByte = 7 // determined by nat encoding in TastyBuffer
  private val maxNumInByte = (1 << payloadBitsPerByte) - 1

  private def simpleNameValue(name: SimpleName): String =
    if name.length == 0 then ""
    else new String(chrs, name.start, name.length)
}
