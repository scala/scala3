package dotty
package tools
package repl

import org.junit.Test
import org.junit.experimental.categories.Category

/** Runs all tests contained in `compiler/test-resources/repl/` */
class ScriptedTests extends ReplTest {

  @Test def replTests = scripts("/repl").foreach(testFile)

  @Category(Array(classOf[BootstrappedOnlyTests]))
  @Test def replMacrosTests = scripts("/repl-macros").foreach(testFile)

  @Test def typePrinterTests = scripts("/type-printer").foreach(testFile)
}

object ScriptedTests {
}
