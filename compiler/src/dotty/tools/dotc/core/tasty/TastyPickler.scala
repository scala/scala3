package dotty.tools
package dotc
package core
package tasty

import dotty.tools.tasty.{TastyBuffer, TastyFormat, TastyHash}
import dotty.tools.tasty.besteffort.BestEffortTastyFormat
import TastyFormat.*
import TastyBuffer.*

import collection.mutable
import core.Symbols.ClassSymbol
import Decorators.*

object TastyPickler:
  private val versionString = s"Scala ${config.Properties.simpleVersionString}"

class TastyPickler(val rootCls: ClassSymbol, isBestEffortTasty: Boolean) {

  private val sections = new mutable.ArrayBuffer[(NameRef, TastyBuffer)]

  val nameBuffer: NameBuffer = new NameBuffer

  def newSection(name: String, buf: TastyBuffer): Unit =
    sections += ((nameBuffer.nameIndex(name.toTermName), buf))

  def assembleParts(): Array[Byte] = {
    def lengthWithLength(buf: TastyBuffer) =
      buf.length + natSize(buf.length)

    nameBuffer.assemble()
    val sectionCount = sections.length
    var i = 0
    while i < sectionCount do
      sections(i)._2.assemble()
      i += 1

    val nameBufferHash = TastyHash.pjwHash64(nameBuffer.bytes, nameBuffer.length)
    val treeSection = sections(0)
    val treeSectionHash = TastyHash.pjwHash64(treeSection._2.bytes, treeSection._2.length)

    // Hash of name table and tree
    val uuidLow: Long = nameBufferHash ^ treeSectionHash
    // Hash of positions, comments and any additional section
    var uuidHi: Long = 0L
    var sectionsSize = natSize(treeSection._1.index) + lengthWithLength(treeSection._2)
    i = 1
    while i < sectionCount do
      val section = sections(i)
      val sectionBuffer = section._2
      uuidHi ^= TastyHash.pjwHash64(sectionBuffer.bytes, sectionBuffer.length)
      sectionsSize += natSize(section._1.index) + lengthWithLength(sectionBuffer)
      i += 1

    val headerBuffer = {
      val fileHeader = if isBestEffortTasty then BestEffortTastyFormat.bestEffortHeader else header
      val buf = new TastyBuffer(fileHeader.length + TastyPickler.versionString.length + 32)
      for (ch <- fileHeader) buf.writeByte(ch.toByte)
      buf.writeNat(MajorVersion)
      buf.writeNat(MinorVersion)
      if isBestEffortTasty then buf.writeNat(BestEffortTastyFormat.PatchVersion)
      buf.writeNat(ExperimentalVersion)
      buf.writeUtf8(TastyPickler.versionString)
      buf.writeUncompressedLong(uuidLow)
      buf.writeUncompressedLong(uuidHi)
      buf
    }

    val totalSize =
      headerBuffer.length +
      lengthWithLength(nameBuffer) +
      sectionsSize
    val all = new TastyBuffer(totalSize)
    all.writeBytes(headerBuffer.bytes, headerBuffer.length)
    all.writeNat(nameBuffer.length)
    all.writeBytes(nameBuffer.bytes, nameBuffer.length)
    i = 0
    while i < sectionCount do
      val section = sections(i)
      val nameRef = section._1
      val buf = section._2
      all.writeNat(nameRef.index)
      all.writeNat(buf.length)
      all.writeBytes(buf.bytes, buf.length)
      i += 1
    assert(all.length == totalSize && all.bytes.length == totalSize, s"totalSize = $totalSize, all.length = ${all.length}, all.bytes.length = ${all.bytes.length}")
    all.bytes
  }
}
