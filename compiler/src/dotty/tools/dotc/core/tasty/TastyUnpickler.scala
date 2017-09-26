package dotty.tools.dotc
package core
package tasty

import scala.collection.mutable
import TastyFormat._
import TastyBuffer.NameRef
import Names.{Name, TermName, termName, EmptyTermName}
import NameKinds._
import java.util.UUID

object TastyUnpickler {
  class UnpickleException(msg: String) extends Exception(msg)

  abstract class SectionUnpickler[R](val name: String) {
    def unpickle(reader: TastyReader, nameAtRef: NameTable): R
  }

  class NameTable extends (NameRef => TermName) {
    private val names = new mutable.ArrayBuffer[TermName]
    def add(name: TermName) = names += name
    def apply(ref: NameRef) = names(ref.index)
    def contents: Iterable[TermName] = names
  }
}

import TastyUnpickler._

class TastyUnpickler(reader: TastyReader) {
  import reader._

  def this(bytes: Array[Byte]) = this(new TastyReader(bytes))

  private val sectionReader = new mutable.HashMap[String, TastyReader]
  val nameAtRef = new NameTable

  private def check(cond: Boolean, msg: => String) =
    if (!cond) throw new UnpickleException(msg)

  private def readName(): TermName = nameAtRef(readNameRef())
  private def readString(): String = readName().toString

  private def readNameContents(): TermName = {
    val tag = readByte()
    val length = readNat()
    val start = currentAddr
    val end = start + length
    val result = tag match {
      case UTF8 =>
        goto(end)
        termName(bytes, start.index, length)
      case QUALIFIED | FLATTENED | EXPANDED | EXPANDPREFIX =>
        qualifiedNameKindOfTag(tag)(readName(), readName().asSimpleName)
      case UNIQUE =>
        val separator = readName().toString
        val num = readNat()
        val originals = until(end)(readName())
        val original = if (originals.isEmpty) EmptyTermName else originals.head
        uniqueNameKindOfSeparator(separator)(original, num)
      case DEFAULTGETTER | VARIANT | OUTERSELECT =>
        numberedNameKindOfTag(tag)(readName(), readNat())
      case SIGNED =>
        val original = readName()
        val result = readName().toTypeName
        val params = until(end)(readName().toTypeName)
        var sig = Signature(params, result)
        if (sig == Signature.NotAMethod) sig = Signature.NotAMethod // needed temporarily, as long as we read old tasty
        original.withSig(sig).asInstanceOf[TermName]
      case _ =>
        simpleNameKindOfTag(tag)(readName())
    }
    assert(currentAddr == end, s"bad name $result $start $currentAddr $end")
    result
  }

  private def readHeader(): UUID = {
    for (i <- 0 until header.length)
      check(readByte() == header(i), "not a TASTy file")
    val major = readNat()
    val minor = readNat()
    check(major == MajorVersion && minor <= MinorVersion,
      s"""TASTy signature has wrong version.
         | expected: $MajorVersion.$MinorVersion
         | found   : $major.$minor""".stripMargin)
    new UUID(readUncompressedLong(), readUncompressedLong())
  }

  private val uuid = readHeader()

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
}
