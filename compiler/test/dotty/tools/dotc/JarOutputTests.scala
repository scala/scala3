package dotty
package tools
package dotc

import java.io.{File => JFile}
import java.nio.file._

import org.junit.Assert._
import org.junit.Test
import vulpix.TestConfiguration.mkClassPath

class JarOutputTests {

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
