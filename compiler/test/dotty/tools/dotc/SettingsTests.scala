package dotty.tools.dotc

import org.junit.Test
import org.junit.Assert._

class SettingsTests {

  @Test def missingOutputDir: Unit = {
    val options = Array("-d", "not_here")
    val reporter = Main.process(options)
    assertEquals(1, reporter.errorCount)
    assertEquals("'not_here' does not exist or is not a directory", reporter.allErrors.head.message)
  }
}
