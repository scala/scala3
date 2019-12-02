package dotty
package tools
package repl

import org.junit.Test

/** Runs all tests contained in `compiler/test-resources/repl/` */
class ScriptedTests extends ReplTest {
  import ScriptedTests._

  @Test def replTests = scripts("/repl").foreach(testFile)

  @Test def typePrinterTests = scripts("/type-printer").foreach(testFile)
}

object ScriptedTests {
}
