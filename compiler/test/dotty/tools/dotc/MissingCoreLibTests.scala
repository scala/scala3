package dotty
package tools
package dotc

import org.junit.Test
import org.junit.Assert._

import vulpix.TestConfiguration.mkClasspath

class MissingCoreLibTests {

  @Test def missingDottyLib: Unit = {
    val classPath = mkClasspath(List(Properties.scalaLibrary)) // missing Properties.dottyLibrary
    val source = "tests/pos/Foo.scala"
    val options = Array("-classpath", classPath, source)
    val reporter = Main.process(options)
    assertEquals(1, reporter.errorCount)
    val errorMessage = reporter.allErrors.head.message
    // FIXME: We currently only detect if the scala library is missing but not the dotty library.
    //        See dotty.tools.dotc.MissingCoreLibraryException
    // assertTrue(errorMessage.contains("Make sure the compiler core libraries are on the classpath"))
  }

}
