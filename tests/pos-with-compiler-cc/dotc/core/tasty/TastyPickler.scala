package dotty.tools
package dotc
package core
package tasty

import scala.language.unsafeNulls

import dotty.tools.tasty.{TastyBuffer, TastyFormat, TastyHash}
import TastyFormat._
import TastyBuffer._

import collection.mutable
import core.Symbols.ClassSymbol
import Decorators._

object TastyPickler {

  private val versionStringBytes = {
    val compilerString = s"Scala ${config.Properties.simpleVersionString}"
    compilerString.getBytes(java.nio.charset.StandardCharsets.UTF_8)
  }

}

class TastyPickler(val rootCls: ClassSymbol) {

  private val sections = new mutable.ArrayBuffer[(NameRef, TastyBuffer)]

  val nameBuffer: NameBuffer = new NameBuffer

  def newSection(name: String, buf: TastyBuffer): Unit =
    sections += ((nameBuffer.nameIndex(name.toTermName), buf))

  def assembleParts(): Array[Byte] = {
    def lengthWithLength(buf: TastyBuffer) =
      buf.length + natSize(buf.length)

    nameBuffer.assemble()
    sections.foreach(_._2.assemble())

    val nameBufferHash = TastyHash.pjwHash64(nameBuffer.bytes)
    val treeSectionHash +: otherSectionHashes = sections.map(x => TastyHash.pjwHash64(x._2.bytes)): @unchecked

    // Hash of name table and tree
    val uuidLow: Long = nameBufferHash ^ treeSectionHash
    // Hash of positions, comments and any additional section
    val uuidHi: Long = otherSectionHashes.fold(0L)(_ ^ _)

    val headerBuffer = {
      val buf = new TastyBuffer(header.length + TastyPickler.versionStringBytes.length + 32)
      for (ch <- header) buf.writeByte(ch.toByte)
      buf.writeNat(MajorVersion)
      buf.writeNat(MinorVersion)
      buf.writeNat(ExperimentalVersion)
      buf.writeNat(TastyPickler.versionStringBytes.length)
      buf.writeBytes(TastyPickler.versionStringBytes, TastyPickler.versionStringBytes.length)
      buf.writeUncompressedLong(uuidLow)
      buf.writeUncompressedLong(uuidHi)
      buf
    }

    val totalSize =
      headerBuffer.length +
      lengthWithLength(nameBuffer) + {
        for ((nameRef, buf) <- sections) yield
          natSize(nameRef.index) + lengthWithLength(buf)
      }.sum
    val all = new TastyBuffer(totalSize)
    all.writeBytes(headerBuffer.bytes, headerBuffer.length)
    all.writeNat(nameBuffer.length)
    all.writeBytes(nameBuffer.bytes, nameBuffer.length)
    for ((nameRef, buf) <- sections) {
      all.writeNat(nameRef.index)
      all.writeNat(buf.length)
      all.writeBytes(buf.bytes, buf.length)
    }
    assert(all.length == totalSize && all.bytes.length == totalSize, s"totalSize = $totalSize, all.length = ${all.length}, all.bytes.length = ${all.bytes.length}")
    all.bytes
  }
}
