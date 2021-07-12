package dotty.tools.tasty

import org.junit.Assert._
import org.junit.{Test, Ignore}

import TastyFormat._
import TastyBuffer._

@Ignore // comment if you want to experiment with error messages
class TastyHeaderUnpicklerTest {

  import TastyHeaderUnpicklerTest._

  @Test def vanilla: Unit = {
    runTest(MajorVersion, MinorVersion, ExperimentalVersion, "Scala 3.0.0-M4-bin-SNAPSHOT-git-12345")
  }

  @Test def failBumpExperimental: Unit = {
    (runTest(MajorVersion, MinorVersion, ExperimentalVersion + 1, "Scala 3.0.0-M4-bin-SNAPSHOT-git-12345"))
  }

  @Test def failBumpMinor: Unit = {
    (runTest(MajorVersion, MinorVersion + 1, ExperimentalVersion, "Scala 3.1.0-RC1"))
  }

  @Test def failBumpMajor: Unit = {
    (runTest(MajorVersion + 1, MinorVersion, ExperimentalVersion, "Scala 4.0.0-M1"))
  }

  @Test def failBumpMajorFinal: Unit = {
    (runTest(MajorVersion + 1, MinorVersion, 0, "Scala 4.0.0"))
  }

  @Test def okSubtractExperimental: Unit = {
    (runTest(MajorVersion, MinorVersion, ExperimentalVersion - 1, "Scala 3.0.0"))
  }

  @Test def okSubtractMinor: Unit = {
    (runTest(MajorVersion, MinorVersion - 1, ExperimentalVersion, "Scala 3.0.0-M4-bin-SNAPSHOT-git-12345"))
  }

  @Test def failSubtractMajor: Unit = {
    (runTest(MajorVersion - 1, MinorVersion, ExperimentalVersion, "Scala 3.0.0-M4-bin-SNAPSHOT-git-12345"))
  }

}

object TastyHeaderUnpicklerTest {


  def fillHeader(maj: Int, min: Int, exp: Int, compiler: String): TastyBuffer = {
    val compilerBytes = compiler.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    val buf = new TastyBuffer(header.length + 32 + compilerBytes.length)
    for (ch <- header) buf.writeByte(ch.toByte)
    buf.writeNat(maj)
    buf.writeNat(min)
    buf.writeNat(exp)
    buf.writeNat(compilerBytes.length)
    buf.writeBytes(compilerBytes, compilerBytes.length)
    buf.writeUncompressedLong(237478L)
    buf.writeUncompressedLong(324789L)
    buf
  }

  def runTest(maj: Int, min: Int, exp: Int, compiler: String): Unit = {
    val headerBuffer = fillHeader(maj, min, exp, compiler)
    val bs = headerBuffer.bytes.clone

    val hr = new TastyHeaderUnpickler(bs)

    hr.readFullHeader()
  }

  def expectUnpickleError(op: => Unit) = {
    try {
      op
      fail()
    }
    catch {
      case err: UnpickleException => ()
    }
  }

}
