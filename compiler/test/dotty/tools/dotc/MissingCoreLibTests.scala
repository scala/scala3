package dotty
package tools
package dotc

import org.junit.Test
import org.junit.Assert._

import vulpix.TestConfiguration.mkClassPath

class MissingCoreLibTests {

  @Test def missingDottyLib: Unit = {
    val classPath = mkClassPath(Jars.dottyCompiler :: Jars.dottyInterfaces :: Jars.dottyExtras) // missing Jars.dottyLib
    val source = "../tests/neg/nolib/Foo.scala"
    val options = Array("-classpath", classPath, "-d", "../out/", source)
    val reporter = Main.process(options)
    assertEquals(1, reporter.errorCount)
    val errorMessage = reporter.allErrors.head.message
    assertTrue(errorMessage.contains("Make sure the compiler core libraries are on the classpath"))
  }

}
