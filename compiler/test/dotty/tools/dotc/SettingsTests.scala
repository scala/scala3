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

  @Test def t8124: Unit =
    val source    = Paths.get("tests/pos/Foo.scala").normalize
    val outputDir = Paths.get("out/testSettings").normalize
    if Files.notExists(outputDir)
      Files.createDirectory(outputDir)
    val options  = Array("-encoding", "-d", outputDir.toString, source.toString)
    val reporter = Main.process(options, reporter = StoreReporter())
    assertEquals(1, reporter.errorCount)
}
