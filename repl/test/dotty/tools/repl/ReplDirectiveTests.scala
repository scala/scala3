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

  @Test def `lone command is incomplete until code follows`: Unit = contextually:
    assertTrue(ParseResult.isIncomplete(":dep com.lihaoyi::os-lib:0.11.3"))

  @Test def `command with code is complete`: Unit = contextually:
    assertFalse(ParseResult.isIncomplete(":dep com.lihaoyi::os-lib:0.11.3\nval p = os.pwd"))

  @Test def `lone command with trailing newline is complete`: Unit = contextually:
    assertFalse(ParseResult.isIncomplete(":dep com.lihaoyi::os-lib:0.11.3\n"))

  @Test def `command with trailing code parses as CommandThenCode`: Unit = initially:
    ParseResult(":type \"hello\"\nval x = 5") match
      case CommandThenCode(TypeOf("\"hello\""), _: Parsed) => // expected
      case other => org.junit.Assert.fail(s"unexpected parse result: $other")

  @Test def `command with trailing code evaluates both`: Unit =
    initially:
      run(":type \"hello\"\nval x = 5")
      val output = storedOutput()
      assertTrue(output, output.contains("String"))
      assertTrue(output, output.contains("val x: Int = 5"))

  @Test def `lone command with trailing newline parses as command`: Unit = initially:
    ParseResult(":type 1\n") match
      case TypeOf("1") => // expected
      case other => org.junit.Assert.fail(s"unexpected parse result: $other")

  @Test def `repeated lone commands with trailing newline work`: Unit =
    initially:
      run(":type 1\n")
      val first = storedOutput()
      assertTrue(first, first.contains("Int"))
      assertFalse(first, first.contains("Illegal start of statement"))
      run(":type 2\n")
      val second = storedOutput()
      assertTrue(second, second.contains("Int"))
      assertFalse(second, second.contains("Illegal start of statement"))

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
