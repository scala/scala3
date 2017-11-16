package dotty.tools.dotc

import org.junit.Test
import org.junit.Assert._

import java.nio.file._

import dotty.Jars
import dotty.tools.vulpix.TestConfiguration.mkClassPath

class SettingsTests {

  @Test def missingOutputDir: Unit = {
    val options = Array("-d", "not_here")
    val reporter = Main.process(options)
    assertEquals(1, reporter.errorCount)
    assertEquals("'not_here' does not exist or is not a directory", reporter.allErrors.head.message)
  }

  @Test def jarOutput: Unit = {
    val classPath = mkClassPath(Jars.dottyTestDeps)
    val source = "../tests/neg/nolib/Foo.scala"
    val out = Paths.get("../out/jaredFoo.jar").normalize
    if (Files.exists(out)) Files.delete(out)
    val options = Array("-classpath", classPath, "-d", out.toString, source)
    val reporter = Main.process(options)
    assertEquals(0, reporter.errorCount)
    assertTrue(Files.exists(out))
  }
}
