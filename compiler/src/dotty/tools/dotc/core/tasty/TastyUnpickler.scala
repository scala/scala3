package dotty.tools.dotc
package core
package tasty

import scala.language.unsafeNulls

import dotty.tools.tasty.{TastyFormat, TastyBuffer, TastyReader, TastyHeaderUnpickler}
import TastyFormat.NameTags._, TastyFormat.nameTagToString
import TastyBuffer.NameRef

import scala.collection.mutable
import Names.{TermName, termName, EmptyTermName}
import NameKinds._

object TastyUnpickler {

  abstract class SectionUnpickler[R](val name: String) {
    def unpickle(reader: TastyReader, nameAtRef: NameTable): R
  }

  class NameTable extends (NameRef => TermName) {
    private val names = new mutable.ArrayBuffer[TermName]
    def add(name: TermName): mutable.ArrayBuffer[TermName] = names += name
    def apply(ref: NameRef): TermName = names(ref.index)
    def contents: Iterable[TermName] = names
  }
}

import TastyUnpickler._

class TastyUnpickler(reader: TastyReader) {
  import reader._

  def this(bytes: Array[Byte]) = this(new TastyReader(bytes))

  private val sectionReader = new mutable.HashMap[String, TastyReader]
  val nameAtRef: NameTable = new NameTable

  private def readName(): TermName = nameAtRef(readNameRef())
  private def readString(): String = readName().toString

  private def readParamSig(): Signature.ParamSig = {
    val ref = readInt()
    if (ref < 0)
      ref.abs
    else
      nameAtRef(NameRef(ref)).toTypeName
  }

  private def readNameContents(): TermName = {
    val tag = readByte()
    val length = readNat()
    val start = currentAddr
    val end = start + length
    def readSignedRest(original: TermName, target: TermName): TermName =
      val result = readName().toTypeName
      // DOTTY: we shouldn't have to give an explicit type to paramsSig,
      // see https://github.com/lampepfl/dotty/issues/4867
      val paramsSig: List[Signature.ParamSig] = until(end)(readParamSig())
      val sig = Signature(paramsSig, result)
      SignedName(original, sig, target)

    val result = tag match {
      case UTF8 =>
        goto(end)
        termName(bytes, start.index, length)
      case QUALIFIED | EXPANDED | EXPANDPREFIX =>
        qualifiedNameKindOfTag(tag)(readName(), readName().asSimpleName)
      case UNIQUE =>
        val separator = readName().toString
        val num = readNat()
        val originals = until(end)(readName())
        val original = if (originals.isEmpty) EmptyTermName else originals.head
        uniqueNameKindOfSeparator(separator)(original, num)
      case DEFAULTGETTER =>
        numberedNameKindOfTag(tag)(readName(), readNat())
      case SIGNED =>
        val original = readName()
        readSignedRest(original, original)
      case TARGETSIGNED =>
        val original = readName()
        val target = readName()
        readSignedRest(original, target)
      case SUPERACCESSOR | INLINEACCESSOR | BODYRETAINER | OBJECTCLASS =>
        simpleNameKindOfTag(tag)(readName())
      case _ =>
        throw MatchError(s"unknown name tag ${nameTagToString(tag)}")
    }
    assert(currentAddr == end, s"bad name $result $start $currentAddr $end")
    result
  }

  new TastyHeaderUnpickler(reader).readHeader()

  locally {
    until(readEnd()) { nameAtRef.add(readNameContents()) }
    while (!isAtEnd) {
      val secName = readString()
      val secEnd = readEnd()
      sectionReader(secName) = new TastyReader(bytes, currentAddr.index, secEnd.index, currentAddr.index)
      goto(secEnd)
    }
  }

  def unpickle[R](sec: SectionUnpickler[R]): Option[R] =
    for (reader <- sectionReader.get(sec.name)) yield
      sec.unpickle(reader, nameAtRef)

  private[dotc] def bytes: Array[Byte] = reader.bytes
}
