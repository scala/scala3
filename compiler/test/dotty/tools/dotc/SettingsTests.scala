package dotty.tools
package dotc

import reporting.StoreReporter
import vulpix.TestConfiguration

import dotty.tools.vulpix.TestConfiguration.mkClasspath

import java.nio.file._

import org.junit.Test
import org.junit.Assert._

class SettingsTests {

  @Test def missingOutputDir: Unit =
    val options = Array("-d", "not_here")
    val reporter = Main.process(options, reporter = StoreReporter())
    assertEquals(1, reporter.errorCount)
    assertEquals("'not_here' does not exist or is not a directory or .jar file", reporter.allErrors.head.message)

  @Test def jarOutput: Unit = {
    val source = "tests/pos/Foo.scala"
    val out = Paths.get("out/jaredFoo.jar").normalize
    if (Files.exists(out)) Files.delete(out)
    val options = Array("-classpath", TestConfiguration.basicClasspath, "-d", out.toString, source)
    val reporter = Main.process(options)
    assertEquals(0, reporter.errorCount)
    assertTrue(Files.exists(out))
  }

  @Test def `t8124 Don't crash on missing argument`: Unit =
    val source    = Paths.get("tests/pos/Foo.scala").normalize
    val outputDir = Paths.get("out/testSettings").normalize
    if Files.notExists(outputDir) then Files.createDirectory(outputDir)
    // -encoding takes an arg!
    val options  = Array("-encoding", "-d", outputDir.toString, source.toString)
    val reporter = Main.process(options, reporter = StoreReporter())
    assertEquals(1, reporter.errorCount)

  //List(dynamics, existentials, higherKinds, implicitConversions, postfixOps, reflectiveCalls, Scala2Compat, noAutoTupling, strictEquality, adhocExtensions, dependent, 3.0-migration, 3.0, 3.1-migration, 3.1, experimental.macros, experimental.dependent)
  @Test def `t9901 Settings detects available language features`: Unit =
    val options = Array("-language:dynamics,strictEquality,experimental.macros")
    val reporter = Main.process(options, reporter = StoreReporter())
    assertEquals(0, reporter.errorCount)

  @Test def `t9901 Settings detects one erroneous language feature`: Unit =
    val options = Array("-language:dynamics,awesome,experimental")
    val reporter = Main.process(options, reporter = StoreReporter())
    assertEquals(1, reporter.errorCount)
    assertEquals("awesome is not a valid choice for -language", reporter.allErrors.head.message)

  @Test def `t9901 Settings excludes experimental`: Unit =
    val options = Array("-language:dynamics,experimental")
    val reporter = Main.process(options, reporter = StoreReporter())
    assertEquals(1, reporter.errorCount)
    assertEquals("experimental is not a valid choice for -language", reporter.allErrors.head.message)

  @Test def `t9901 Settings includes funky strings`: Unit =
    val options = Array("-language:3.1-migration")
    val reporter = Main.process(options, reporter = StoreReporter())
    assertEquals(0, reporter.errorCount)

}
