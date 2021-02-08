package dotty.tools.tasty

import java.util.UUID

import TastyFormat.{MajorVersion, MinorVersion, ExperimentalVersion, header}

/**
 * The Tasty Header consists of four fields:
 * - uuid
 *   - contains a hash of the sections of the TASTy file
 * - majorVersion
 *   - matching the TASTy format version that last broke backwards compatibility
 * - minorVersion
 *   - matching the TASTy format version that last broke forward compatibility
 * - experimentalVersion
 *   - 0 for final compiler version
 *   - positive for between minor versions and forward compatibility
 *     is broken since the previous stable version.
 * - toolingVersion
 *   - arbitrary string representing the tooling that produced the TASTy
 */
sealed abstract case class TastyHeader(
  uuid: UUID,
  majorVersion: Int,
  minorVersion: Int,
  experimentalVersion: Int,
  toolingVersion: String // String could lead to a lot of duplication, perhaps cache headers?
)

class TastyHeaderUnpickler(reader: TastyReader) {
  import TastyHeaderUnpickler._
  import reader._

  def this(bytes: Array[Byte]) = this(new TastyReader(bytes))

  /** reads and verifies the TASTy version, extracting the UUID */
  def readHeader(): UUID =
    readFullHeader().uuid

  /** reads and verifies the TASTy version, extracting the whole header */
  def readFullHeader(): TastyHeader = {

    for (i <- 0 until header.length)
      check(readByte() == header(i), "not a TASTy file")
    val fileMajor = readNat()
    if (fileMajor <= 27) { // old behavior before `tasty-core` 3.0.0-M4
      val fileMinor = readNat()
      val signature = signatureString(expectedMinor, fileMajor, fileMinor.toString)
      throw new UnpickleException(signature + backIncompatAddendum + myAddendum)
    }
    else {
      val fileMinor = readNat()
      val fileExperimental = readNat()
      val toolingVersion = {
        val length = readNat()
        val start = currentAddr
        val end = start + length
        goto(end)
        new String(bytes, start.index, length) // NOTE: new string object here (should probably cache as it leaks)
      }

      val validVersion = ( // inlined from `dotty.tools.tasty.TastyVersionFormatTest.TastyVersion.<:<`
        fileMajor == MajorVersion && (
          if (fileExperimental == ExperimentalVersion) {
            if (ExperimentalVersion == 0) {
              fileMinor <= MinorVersion
            }
            else {
              fileMinor == MinorVersion
            }
          }
          else {
            fileExperimental == 0 && fileMinor < MinorVersion
          }
        )
      )
      check(validVersion, {
        val foundMinor = showMinorVersion(fileMinor, fileExperimental)
        val producedByAddendum = s"\nThe TASTy file was produced by $toolingVersion.$myAddendum"
        val signature = signatureString(expectedMinor, fileMajor, foundMinor)
        if (fileExperimental != 0)
          signature + unstableAddendum + producedByAddendum
        else if (fileMajor < MajorVersion)
          signature + backIncompatAddendum + producedByAddendum
        else
          signature + forwardIncompatAddendum + producedByAddendum
      }
      )
      val uuid = new UUID(readUncompressedLong(), readUncompressedLong())
      new TastyHeader(uuid, fileMajor, fileMinor, fileExperimental, toolingVersion) {}
    }
  }

  def isAtEnd: Boolean = reader.isAtEnd

  private def check(cond: Boolean, msg: => String): Unit = {
    if (!cond) throw new UnpickleException(msg)
  }
}

object TastyHeaderUnpickler {

  private def showMinorVersion(min: Int, exp: Int) = {
    val expStr = if (exp == 0) "" else s" [unstable release: $exp]"
    s"$min$expStr"
  }

  private def expectedMinor = showMinorVersion(MinorVersion, ExperimentalVersion)

  private def myAddendum = {
    if (ExperimentalVersion > 0)
      "\nNote that your tooling is currently using an unstable TASTy version."
    else
      ""
  }

  private def signatureString(minorVersion: String, fileMajor: Int, fileMinor: String) = (
    s"""TASTy signature has wrong version.
      | expected: {majorVersion: $MajorVersion, minorVersion: $minorVersion}
      | found   : {majorVersion: $fileMajor, minorVersion: $fileMinor}
      |
      |""".stripMargin
  )

  private def unstableAddendum =
    """This TASTy file was produced by an unstable release.
      |To read this TASTy file, your tooling must be at the same version.""".stripMargin

  private def backIncompatAddendum =
    """This TASTy file was produced by a backwards incompatible release.
      |Please recompile this TASTy with a later version.""".stripMargin

  private def forwardIncompatAddendum =
    """This TASTy file was produced by a forwards incompatible release.
      |To read this TASTy file, please upgrade your tooling.""".stripMargin
}
