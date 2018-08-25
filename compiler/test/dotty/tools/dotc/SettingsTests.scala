package dotty.tools
package dotc

import vulpix.TestConfiguration

import org.junit.Test
import org.junit.Assert._

import java.nio.file._

import dotty.tools.vulpix.TestConfiguration.mkClasspath

class SettingsTests {

  @Test def missingOutputDir: Unit = {
    val options = Array("-d", "not_here")
    val reporter = Main.process(options)
    assertEquals(1, reporter.errorCount)
    assertEquals("'not_here' does not exist or is not a directory or .jar file", reporter.allErrors.head.message)
  }

  @Test def jarOutput: Unit = {
    val source = "tests/pos/Foo.scala"
    val out = Paths.get("out/jaredFoo.jar").normalize
    if (Files.exists(out)) Files.delete(out)
    val options = Array("-classpath", TestConfiguration.basicClasspath, "-d", out.toString, source)
    val reporter = Main.process(options)
    assertEquals(0, reporter.errorCount)
    assertTrue(Files.exists(out))
  }
}
