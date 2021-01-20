package dotty.tools.tasty

import java.util.UUID

import TastyFormat.{MajorVersion, MinorVersion, header}

sealed abstract case class TastyHeader(uuid: UUID, majorVersion: Int, minorVersion: Int)

class TastyHeaderUnpickler(reader: TastyReader) {
  import reader._

  def this(bytes: Array[Byte]) = this(new TastyReader(bytes))

  /** delegates to `readFullHeader`, extracting the UUID */
  def readHeader(): UUID =
    readFullHeader().uuid

  /**Reads the header of a Tasty File, the returned header, `h`,
   * has the following properties:
   * - `h.majorVersion == TastyFormat.MajorVersion`
   * - `0 <= h.minorVersion <= TastyFormat.MinorVersion`
   */
  def readFullHeader(): TastyHeader = {
    for (i <- 0 until header.length)
      check(readByte() == header(i), "not a TASTy file")
    val majorVersion = readNat()
    val minorVersion = readNat()
    val validVersion = (
      majorVersion == MajorVersion
      && minorVersion >= 0 && minorVersion <= MinorVersion
    )
    check(validVersion,
      s"""TASTy signature has wrong version.
         | expected: $MajorVersion.$MinorVersion
         | found   : ${majorVersion}.${minorVersion}""".stripMargin
    )
    val uuid = new UUID(readUncompressedLong(), readUncompressedLong())
    new TastyHeader(uuid, majorVersion, minorVersion) {}
  }

  def isAtEnd: Boolean = reader.isAtEnd

  private def check(cond: Boolean, msg: => String): Unit = {
    if (!cond) throw new UnpickleException(msg)
  }
}
