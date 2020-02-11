package dotty.tools
package dottydoc

import org.junit.Test
import org.junit.Assert._

class SettingsTests {

  @Test def t8124: Unit = {
    val source = "tests/pos/Foo.scala"
    val url = "https://github.com/lampepfl/dotty/tree/master/tests"
    val options = Array("-project", "-project-url", url, source)
    val reporter = Main.process(options)
    assertEquals(2, reporter.errorCount)
    assertEquals("missing argument for option -project", reporter.allErrors.last.message)
  }

}
