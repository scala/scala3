package dotty.tools
package repl

import org.junit.Assert.{assertFalse, assertTrue}
import org.junit.Test

class ReplDirectiveTests extends ReplTest:

  @Test def `unsupported options directive warns`: Unit =
    initially:
      run("//> using options -Werror")
      val output = storedOutput()
      assertTrue(output, output.contains("using options"))
      assertTrue(output, output.contains("not supported in the REPL"))
      assertFalse(output, output.contains("Resolved"))

  @Test def `unsupported scala directive warns`: Unit =
    initially:
      run("//> using scala 3.3.1")
      val output = storedOutput()
      assertTrue(output, output.contains("using scala"))
      assertTrue(output, output.contains("not supported in the REPL"))

  @Test def `dep directive is treated as :dep alias`: Unit =
    initially:
      run("//> using dep some.bogus.coords")
      val output = storedOutput()
      assertFalse(output, output.contains("not supported in the REPL"))
