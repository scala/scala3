package dotty.tools
package repl

import org.junit.Assert.{assertFalse, assertTrue}
import org.junit.Test

class ReplDirectiveTests extends ReplTest:

  @Test def `lone dep directive is incomplete until code follows`: Unit = contextually:
    assertTrue(ParseResult.isIncomplete("//> using dep com.lihaoyi::os-lib:0.11.3"))

  @Test def `dep directive with code is complete`: Unit = contextually:
    assertFalse(ParseResult.isIncomplete("//> using dep com.lihaoyi::os-lib:0.11.3\nval p = os.pwd"))

  @Test def `lone dep directive with trailing newline is complete`: Unit = contextually:
    assertFalse(ParseResult.isIncomplete("//> using dep com.lihaoyi::os-lib:0.11.3\n"))

  @Test def `multiple dep directives without code are incomplete`: Unit = contextually:
    assertTrue(ParseResult.isIncomplete(
      "//> using dep com.lihaoyi::upickle:4.4.3\n//> using dep com.lihaoyi::os-lib:0.11.3"))

  @Test def `multiple dep directives with code are complete`: Unit = contextually:
    assertFalse(ParseResult.isIncomplete(
      "//> using dep com.lihaoyi::upickle:4.4.3\n//> using dep com.lihaoyi::os-lib:0.11.3\nval p = os.pwd"))

  @Test def `unsupported directive with code is complete`: Unit = contextually:
    assertFalse(ParseResult.isIncomplete("//> using options -Werror\nval x = 1"))

  @Test def `plain comment is complete`: Unit = contextually:
    assertFalse(ParseResult.isIncomplete("// just a comment"))

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

  @Test def `unsupported directive before code still evaluates code`: Unit =
    initially:
      run("//> using options -Werror\nval x = 1")
      val output = storedOutput()
      assertTrue(output, output.contains("not supported in the REPL"))
      assertTrue(output, output.contains("val x: Int = 1"))

  @Test def `unsupported directive before expression still evaluates expression`: Unit =
    initially:
      run("//> using scala 3.3.1\n1 + 1")
      val output = storedOutput()
      assertTrue(output, output.contains("not supported in the REPL"))
      assertTrue(output, output.contains("val res0: Int = 2"))

  @Test def `directive after code is not treated as command`: Unit =
    initially:
      run("val y = 2\n//> using dep some.bogus.coords")
      val output = storedOutput()
      assertTrue(output, output.contains("val y: Int = 2"))
      assertFalse(output, output.contains("not supported in the REPL"))

  @Test def `multiple unsupported directives before code warn and evaluate code`: Unit =
    initially:
      run("//> using options -Werror\n//> using scala 3.3.1\nval z = 3")
      val output = storedOutput()
      assertTrue(output, output.contains("using options"))
      assertTrue(output, output.contains("using scala"))
      assertTrue(output, output.contains("not supported in the REPL"))
      assertTrue(output, output.contains("val z: Int = 3"))
