package dotty.tools.dotc.core.tasty

import org.junit.Assert._
import org.junit.{Test, Ignore}

import dotty.tools.tasty.TastyFormat._
import dotty.tools.tasty.TastyBuffer._
import dotty.tools.tasty.TastyBuffer
import dotty.tools.tasty.TastyReader
import dotty.tools.tasty.UnpickleException
import dotty.tools.tasty.TastyHeaderUnpickler
import dotty.tools.tasty.TastyHeaderUnpickler.TastyVersion
import dotty.tools.tasty.UnpicklerConfig

class TastyHeaderUnpicklerTest {

  import TastyHeaderUnpicklerTest._

  @Test
  def okThisCompilerReadsItself: Unit = {
    val file = TastyVersion(MajorVersion, MinorVersion, ExperimentalVersion)
    val read = TastyVersion(MajorVersion, MinorVersion, ExperimentalVersion)
    runTest(file, read, "Scala (current)")
  }

  @Test
  def okExperimentalCompilerReadsItself: Unit = {
    val file = TastyVersion(MajorVersion, MinorVersion, 1)
    val read = TastyVersion(MajorVersion, MinorVersion, 1)
    runTest(file, read, "Scala (current)")
  }

  @Test
  def okStableCompilerReadsItself: Unit = {
    val file = TastyVersion(MajorVersion, MinorVersion, 0)
    val read = TastyVersion(MajorVersion, MinorVersion, 0)
    runTest(file, read, "Scala (current)")
  }

  @Test
  def okReadOldStableMinorFromStable: Unit = {
    val file = TastyVersion(28, 2, 0)
    val read = TastyVersion(28, 3, 0)
    runTest(file, read, "Scala 3.2.2")
  }

  @Test
  def okReadOldStableMinorFromExperimental: Unit = {
    val file = TastyVersion(28, 2, 0)
    val read = TastyVersion(28, 3, 1)
    runTest(file, read, "Scala 3.2.2")
  }

  @Test
  def failReadExperimentalFromStableSameMinor: Unit = {
    val file = TastyVersion(28, 4, 1)
    val read = TastyVersion(28, 4, 0)
    expectUnpickleError(runTest(file, read, "Scala 3.4.0-RC1-bin-SNAPSHOT")) {
      """Backward incompatible TASTy file has version 28.4-experimental-1, produced by Scala 3.4.0-RC1-bin-SNAPSHOT,
        |  expected stable TASTy from 28.0 to 28.4.
        |  The source of this file should be recompiled by a Scala 3.4.0 compiler or newer.
        |  Usually this means that the library dependency containing this file should be updated.
        |  Please refer to the documentation for information on TASTy versioning:
        |  https://docs.scala-lang.org/scala3/reference/language-versions/binary-compatibility.html""".stripMargin
    }
  }

  @Test
  def failReadExperimentalFromOldMinor: Unit = {
    val file = TastyVersion(28, 3, 1)
    val read = TastyVersion(28, 4, 0)
    expectUnpickleError(runTest(file, read, "Scala 3.2.1-RC1-bin-SNAPSHOT")) {
      """Backward incompatible TASTy file has version 28.3-experimental-1, produced by Scala 3.2.1-RC1-bin-SNAPSHOT,
        |  expected stable TASTy from 28.0 to 28.4.
        |  The source of this file should be recompiled by a Scala 3.3.0 compiler or newer.
        |  Usually this means that the library dependency containing this file should be updated.
        |  Please refer to the documentation for information on TASTy versioning:
        |  https://docs.scala-lang.org/scala3/reference/language-versions/binary-compatibility.html""".stripMargin
    }
  }

  @Test
  def failReadOldMajor: Unit = {
    val file = TastyVersion(27, 3, 0)
    val read = TastyVersion(28, 3, 0)
    expectUnpickleError(runTest(file, read, "Scala 3.0.0-M1")) {
      """Backward incompatible TASTy file has version 27.3,
        |  expected stable TASTy from 28.0 to 28.3.
        |  The source of this file should be recompiled by a Scala 3.0.0 compiler or newer.
        |  Usually this means that the library dependency containing this file should be updated.
        |  Please refer to the documentation for information on TASTy versioning:
        |  https://docs.scala-lang.org/scala3/reference/language-versions/binary-compatibility.html""".stripMargin
    }
  }

  @Test
  def failReadOldMajor_generic: Unit = {
    // We check the generic version here because it will produce a different message.
    val file = TastyVersion(27, 3, 0)
    val read = TastyVersion(28, 3, 0)
    expectUnpickleError(runTest(file, read, "Scala 3.0.0-M1", generic = true)) {
      """Backward incompatible TASTy file has version 27.3,
        |  expected stable TASTy from 28.0 to 28.3.
        |  The source of this file should be recompiled by a later version.
        |  Usually this means that the classpath entry of this file should be updated.
        |  Please refer to the documentation for information on TASTy versioning:
        |  https://docs.scala-lang.org/scala3/reference/language-versions/binary-compatibility.html""".stripMargin
    }
  }

  @Test
  def failReadOldExperimentalFromSameMinorWhileExperimental: Unit = {
    val file = TastyVersion(28, 4, 1)
    val read = TastyVersion(28, 4, 2)
    expectUnpickleError(runTest(file, read, "Scala 3.3.3-RC1-NIGHTLY")) {
      """Backward incompatible TASTy file has version 28.4-experimental-1, produced by Scala 3.3.3-RC1-NIGHTLY,
        |  expected stable TASTy from 28.0 to 28.3, or exactly 28.4-experimental-2.
        |  The source of this file should be recompiled by the same nightly or snapshot Scala 3.3 compiler.
        |  Usually this means that the library dependency containing this file should be updated.
        |  Please refer to the documentation for information on TASTy versioning:
        |  https://docs.scala-lang.org/scala3/reference/language-versions/binary-compatibility.html""".stripMargin
    }
  }

  @Test
  def failReadOldExperimentalFromSameMinorWhileExperimental_generic: Unit = {
    // We check the generic version here because it will produce a different message.
    val file = TastyVersion(28, 4, 1)
    val read = TastyVersion(28, 4, 2)
    expectUnpickleError(runTest(file, read, "Scala 3.3.3-RC1-NIGHTLY", generic = true)) {
      """Backward incompatible TASTy file has version 28.4-experimental-1, produced by Scala 3.3.3-RC1-NIGHTLY,
        |  expected stable TASTy from 28.0 to 28.3, or exactly 28.4-experimental-2.
        |  The source of this file should be recompiled by a later version.
        |  Usually this means that the classpath entry of this file should be updated.
        |  Please refer to the documentation for information on TASTy versioning:
        |  https://docs.scala-lang.org/scala3/reference/language-versions/binary-compatibility.html""".stripMargin
    }
  }

  @Test
  def failReadNewerStableMinorFromStable: Unit = {
    val file = TastyVersion(28, 3, 0)
    val read = TastyVersion(28, 2, 0)
    expectUnpickleError(runTest(file, read, "Scala 3.3.1")) {
      """Forward incompatible TASTy file has version 28.3, produced by Scala 3.3.1,
        |  expected stable TASTy from 28.0 to 28.2.
        |  To read this TASTy file, use a Scala 3.3.0 compiler or newer.
        |  Please refer to the documentation for information on TASTy versioning:
        |  https://docs.scala-lang.org/scala3/reference/language-versions/binary-compatibility.html""".stripMargin
    }
  }

  @Test
  def failReadNewerStableMinorFromStable_generic: Unit = {
    // We check the generic version here because it will produce a different message.
    val file = TastyVersion(28, 3, 0)
    val read = TastyVersion(28, 2, 0)
    expectUnpickleError(runTest(file, read, "Scala 3.3.1", generic = true)) {
      """Forward incompatible TASTy file has version 28.3, produced by Scala 3.3.1,
        |  expected stable TASTy from 28.0 to 28.2.
        |  To read this TASTy file, use a newer version of this tool compatible with TASTy 28.3.
        |  Please refer to the documentation for information on TASTy versioning:
        |  https://docs.scala-lang.org/scala3/reference/language-versions/binary-compatibility.html""".stripMargin
    }
  }

  @Test
  def failReadNewerExperimentalMinorFromStable: Unit = {
    val file = TastyVersion(28, 3, 1)
    val read = TastyVersion(28, 2, 0)
    expectUnpickleError(runTest(file, read, "Scala 3.2.2-RC1-NIGHTLY")) {
      """Forward incompatible TASTy file has version 28.3-experimental-1, produced by Scala 3.2.2-RC1-NIGHTLY,
        |  expected stable TASTy from 28.0 to 28.2.
        |  To read this experimental TASTy file, use the same nightly or snapshot Scala 3.2 compiler.
        |  Note that you are using a stable compiler, which can not read experimental TASTy.
        |  Please refer to the documentation for information on TASTy versioning:
        |  https://docs.scala-lang.org/scala3/reference/language-versions/binary-compatibility.html""".stripMargin
    }
  }

  @Test
  def failReadNewerStableMajor: Unit = {
    val file = TastyVersion(29, 0, 0)
    val read = TastyVersion(28, 3, 0)
    expectUnpickleError(runTest(file, read, "Scala 4.0.0")) {
      """Forward incompatible TASTy file has version 29.0, produced by Scala 4.0.0,
        |  expected stable TASTy from 28.0 to 28.3.
        |  To read this TASTy file, use a more recent Scala compiler.
        |  Please refer to the documentation for information on TASTy versioning:
        |  https://docs.scala-lang.org/scala3/reference/language-versions/binary-compatibility.html""".stripMargin
    }
  }

  @Test
  def failReadNewerExperimentalMajor: Unit = {
    val file = TastyVersion(29, 0, 1)
    val read = TastyVersion(28, 3, 0)
    expectUnpickleError(runTest(file, read, "Scala 4.0.0-M1")) {
      """Forward incompatible TASTy file has version 29.0-experimental-1, produced by Scala 4.0.0-M1,
        |  expected stable TASTy from 28.0 to 28.3.
        |  To read this experimental TASTy file, use the same Scala compiler.
        |  Note that you are using a stable compiler, which can not read experimental TASTy.
        |  Please refer to the documentation for information on TASTy versioning:
        |  https://docs.scala-lang.org/scala3/reference/language-versions/binary-compatibility.html""".stripMargin
    }
  }

  @Test
  def failReadNewerExperimentalMajor_generic: Unit = {
    // We check the generic version here because it will produce a different message.
    val file = TastyVersion(29, 0, 1)
    val read = TastyVersion(28, 3, 0)
    expectUnpickleError(runTest(file, read, "Scala 4.0.0-M1", generic = true)) {
      """Forward incompatible TASTy file has version 29.0-experimental-1, produced by Scala 4.0.0-M1,
        |  expected stable TASTy from 28.0 to 28.3.
        |  To read this experimental TASTy file, use the version of this tool compatible with TASTy 29.0-experimental-1.
        |  Note that this tool does not support reading experimental TASTy.
        |  Please refer to the documentation for information on TASTy versioning:
        |  https://docs.scala-lang.org/scala3/reference/language-versions/binary-compatibility.html""".stripMargin
    }
  }

  @Test
  def failReadStableFromExperimentalSameMinor: Unit = {
    val file = TastyVersion(28, 4, 0)
    val read = TastyVersion(28, 4, 1) // 3.4.0-RC1-NIGHTLY
    expectUnpickleError(runTest(file, read, "Scala 3.4.2")) {
      """Forward incompatible TASTy file has version 28.4, produced by Scala 3.4.2,
        |  expected stable TASTy from 28.0 to 28.3, or exactly 28.4-experimental-1.
        |  To read this TASTy file, use a Scala 3.4.0 compiler or newer.
        |  Please refer to the documentation for information on TASTy versioning:
        |  https://docs.scala-lang.org/scala3/reference/language-versions/binary-compatibility.html""".stripMargin
    }
  }

  @Test
  def failReadNewerExperimentalFromExperimentalSameMinor: Unit = {
    val file = TastyVersion(28, 4, 2)
    val read = TastyVersion(28, 4, 1)
    expectUnpickleError(runTest(file, read, "Scala 3.3.3-RC2-NIGHTLY")) {
      """Forward incompatible TASTy file has version 28.4-experimental-2, produced by Scala 3.3.3-RC2-NIGHTLY,
        |  expected stable TASTy from 28.0 to 28.3, or exactly 28.4-experimental-1.
        |  To read this experimental TASTy file, use the same nightly or snapshot Scala 3.3 compiler.
        |  Please refer to the documentation for information on TASTy versioning:
        |  https://docs.scala-lang.org/scala3/reference/language-versions/binary-compatibility.html""".stripMargin
    }
  }

  @Test
  def failReadNewerExperimentalFromExperimentalSameMinor_generic: Unit = {
    // We check the generic version here because it will produce a different message.
    val file = TastyVersion(28, 4, 2)
    val read = TastyVersion(28, 4, 1)
    expectUnpickleError(runTest(file, read, "Scala 3.3.3-RC2-NIGHTLY", generic = true)) {
      """Forward incompatible TASTy file has version 28.4-experimental-2, produced by Scala 3.3.3-RC2-NIGHTLY,
        |  expected stable TASTy from 28.0 to 28.3, or exactly 28.4-experimental-1.
        |  To read this experimental TASTy file, use the version of this tool compatible with TASTy 28.4-experimental-2.
        |  Please refer to the documentation for information on TASTy versioning:
        |  https://docs.scala-lang.org/scala3/reference/language-versions/binary-compatibility.html""".stripMargin
    }
  }

}

object TastyHeaderUnpicklerTest {

  def fillHeader(maj: Int, min: Int, exp: Int, compiler: String): TastyBuffer = {
    val compilerBytes = compiler.getBytes(java.nio.charset.StandardCharsets.UTF_8).nn
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

  case class CustomScalaConfig(compilerVersion: TastyVersion) extends TastyUnpickler.Scala3CompilerConfig {
    override def majorVersion: Int = compilerVersion.major
    override def minorVersion: Int = compilerVersion.minor
    override def experimentalVersion: Int = compilerVersion.experimental
  }

  case class CustomGenericConfig(compilerVersion: TastyVersion) extends UnpicklerConfig.Generic {
    override def majorVersion: Int = compilerVersion.major
    override def minorVersion: Int = compilerVersion.minor
    override def experimentalVersion: Int = compilerVersion.experimental
  }

  def runTest(file: TastyVersion, read: TastyVersion, compiler: String, generic: Boolean = false): Unit = {
    val headerBuffer = fillHeader(file.major, file.minor, file.experimental, compiler)
    val bs = headerBuffer.bytes.clone
    val config = if (generic) CustomGenericConfig(read) else CustomScalaConfig(read)
    val hr = new TastyHeaderUnpickler(config, new TastyReader(bs))
    hr.readFullHeader()
  }

  def expectUnpickleError(op: => Unit)(message: String) = {
    try {
      op
      fail()
    }
    catch {
      case err: UnpickleException => assert(err.getMessage.nn.contains(message))
    }
  }

}
