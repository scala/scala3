package dotty.tools
package repl

import org.junit.Assert.{assertFalse, assertTrue}
import org.junit.Test

class ReplDirectiveTests extends ReplTest:

  @Test def `lone dep directive is incomplete until code follows`: Unit = contextually:
    assertTrue(ParseResult.onlyPreambleSoFar("//> using dep com.lihaoyi::os-lib:0.11.3"))
    assertFalse(ParseResult.isIncomplete("//> using dep com.lihaoyi::os-lib:0.11.3"))

  @Test def `dep directive with code is complete`: Unit = contextually:
    assertFalse(ParseResult.onlyPreambleSoFar("//> using dep com.lihaoyi::os-lib:0.11.3\nval p = os.pwd"))
    assertFalse(ParseResult.isIncomplete("//> using dep com.lihaoyi::os-lib:0.11.3\nval p = os.pwd"))

  @Test def `lone dep directive with trailing newline is complete`: Unit = contextually:
    assertTrue(ParseResult.shouldAcceptLine("//> using dep com.lihaoyi::os-lib:0.11.3\n", hasPendingInput = false))
    assertFalse(ParseResult.isIncomplete("//> using dep com.lihaoyi::os-lib:0.11.3\n"))

  @Test def `multiple dep directives without code are incomplete`: Unit = contextually:
    assertTrue(ParseResult.onlyPreambleSoFar(
      "//> using dep com.lihaoyi::upickle:4.4.3\n//> using dep com.lihaoyi::os-lib:0.11.3"))
    assertFalse(ParseResult.isIncomplete(
      "//> using dep com.lihaoyi::upickle:4.4.3\n//> using dep com.lihaoyi::os-lib:0.11.3"))

  @Test def `multiple dep directives with code are complete`: Unit = contextually:
    assertFalse(ParseResult.onlyPreambleSoFar(
      "//> using dep com.lihaoyi::upickle:4.4.3\n//> using dep com.lihaoyi::os-lib:0.11.3\nval p = os.pwd"))
    assertFalse(ParseResult.isIncomplete(
      "//> using dep com.lihaoyi::upickle:4.4.3\n//> using dep com.lihaoyi::os-lib:0.11.3\nval p = os.pwd"))

  @Test def `unsupported directive with code is complete`: Unit = contextually:
    assertFalse(ParseResult.onlyPreambleSoFar("//> using options -Werror\nval x = 1"))
    assertFalse(ParseResult.isIncomplete("//> using options -Werror\nval x = 1"))

  @Test def `plain comment is complete`: Unit = contextually:
    assertFalse(ParseResult.onlyPreambleSoFar("// just a comment"))
    assertFalse(ParseResult.isIncomplete("// just a comment"))

  @Test def `lone command is incomplete until code follows`: Unit = contextually:
    assertTrue(ParseResult.onlyPreambleSoFar(":dep com.lihaoyi::os-lib:0.11.3"))
    assertFalse(ParseResult.isIncomplete(":dep com.lihaoyi::os-lib:0.11.3"))

  @Test def `command with code is complete`: Unit = contextually:
    assertFalse(ParseResult.onlyPreambleSoFar(":dep com.lihaoyi::os-lib:0.11.3\nval p = os.pwd"))
    assertFalse(ParseResult.isIncomplete(":dep com.lihaoyi::os-lib:0.11.3\nval p = os.pwd"))

  @Test def `lone command with trailing newline is complete`: Unit = contextually:
    assertTrue(ParseResult.shouldAcceptLine(":dep com.lihaoyi::os-lib:0.11.3\n", hasPendingInput = false))
    assertFalse(ParseResult.isIncomplete(":dep com.lihaoyi::os-lib:0.11.3\n"))

  @Test def `command lines with newline still defer when paste is pending`: Unit = contextually:
    assertFalse(ParseResult.shouldAcceptLine(":settings -old-syntax:false\n", hasPendingInput = true))
    assertTrue(ParseResult.shouldAcceptLine(":settings -old-syntax:false\n", hasPendingInput = false))
    assertFalse(ParseResult.shouldAcceptLine(
      ":settings -old-syntax:false\n:settings -old-syntax:true\n", hasPendingInput = true))
    assertTrue(ParseResult.shouldAcceptLine(
      ":settings -old-syntax:false\n:settings -old-syntax:true\n", hasPendingInput = false))

  @Test def `command with incomplete trailing code is incomplete`: Unit = contextually:
    assertTrue(ParseResult.isIncomplete(":settings -deprecation\nif true then"))
    assertFalse(ParseResult.shouldAcceptLine(":settings -deprecation\nif true then", hasPendingInput = false))

  @Test def `command with complete trailing code is complete`: Unit = contextually:
    assertFalse(ParseResult.isIncomplete(":settings -deprecation\nif true then ()"))
    assertTrue(ParseResult.shouldAcceptLine(":settings -deprecation\nif true then ()", hasPendingInput = false))

  @Test def `code with incomplete trailing expression is incomplete`: Unit = contextually:
    assertTrue(ParseResult.isIncomplete("val x = 5\nif x == 5 then"))
    assertFalse(ParseResult.shouldAcceptLine("val x = 5\nif x == 5 then", hasPendingInput = false))

  @Test def `code with complete trailing expression is complete`: Unit = contextually:
    assertFalse(ParseResult.isIncomplete("val x = 5\nif x == 5 then 1"))
    assertTrue(ParseResult.shouldAcceptLine("val x = 5\nif x == 5 then 1", hasPendingInput = false))

  @Test def `command with trailing code parses as CommandThenCode`: Unit = initially:
    ParseResult(":type \"hello\"\nval x = 5") match
      case CommandThenCode(TypeOf("\"hello\""), code) if code.contains("val x = 5") => // expected
      case other => org.junit.Assert.fail(s"unexpected parse result: $other")

  @Test def `command with trailing code evaluates both`: Unit =
    initially:
      run(":type \"hello\"\nval x = 5")
      val output = storedOutput()
      assertTrue(output, output.contains("String"))
      assertTrue(output, output.contains("val x: Int = 5"))

  @Test def `settings command affects trailing code`: Unit =
    initially:
      run(":settings -old-syntax\nif true then 1 else 2")
      val output = storedOutput()
      assertTrue(output, output.contains("This construct is not allowed under -old-syntax"))

  @Test def `settings command affects subsequent separate input`: Unit =
    initially {
      run(":settings -old-syntax")
    } andThen {
      storedOutput()
      run("if true then 1 else 2")
      val output = storedOutput()
      assertTrue(output, output.contains("This construct is not allowed under -old-syntax"))
    }

  @Test def `settings then another command then code uses updated settings`: Unit =
    initially:
      run(":settings -old-syntax\n:type 1\nif true then 1 else 2")
      val output = storedOutput()
      assertTrue(output, output.contains("Int"))
      assertTrue(output, output.contains("This construct is not allowed under -old-syntax"))

  @Test def `settings can disable old-syntax for trailing code`: Unit =
    initially {
      run(":settings -old-syntax")
    } andThen {
      storedOutput()
      run(":settings -old-syntax:false\nif true then 1 else 2")
      val output = storedOutput()
      assertTrue(output, output.contains("val res0: Int = 1"))
      assertFalse(output, output.contains("This construct is not allowed under -old-syntax"))
    }

  @Test def `conflicting settings in same block apply to trailing code`: Unit =
    initially:
      run(":settings -old-syntax:false\n:settings -old-syntax:true\nif true then println(\"REPL_SETTINGS_MARKER\")")
      val output = storedOutput()
      assertTrue(
        s"trailing code should be rejected under conflicting -old-syntax:true, got:\n$output",
        output.contains("This construct is not allowed under -old-syntax"))
      assertFalse(
        s"trailing code should not run, got:\n$output",
        output.linesIterator.map(_.trim).contains("REPL_SETTINGS_MARKER"))

  @Test def `conflicting settings in same block stay applied on next input`: Unit =
    initially {
      run(":settings -old-syntax:false\n:settings -old-syntax:true\nif true then println(\"REPL_SETTINGS_MARKER\")")
    } andThen {
      storedOutput()
      run("if true then println(\"REPL_SETTINGS_MARKER\")")
      val output = storedOutput()
      assertTrue(
        s"subsequent input should still be rejected under -old-syntax, got:\n$output",
        output.contains("This construct is not allowed under -old-syntax"))
      assertFalse(
        s"subsequent input should not run, got:\n$output",
        output.linesIterator.map(_.trim).contains("REPL_SETTINGS_MARKER"))
    }

  @Test def `conflicting settings alone update state immediately`: Unit =
    initially {
      val state = run(":settings -old-syntax:false\n:settings -old-syntax:true")
      assertTrue(
        "oldSyntax should be true after conflicting :settings in the same block",
        state.context.settings.oldSyntax.value(using state.context))
    }

  /** Simulates JLine accepting the first `:settings` line alone, then a follow-up
   *  paste/submission that contains the conflicting `:settings` plus trailing code.
   */
  @Test def `conflicting settings after prior settings apply to trailing code`: Unit =
    initially {
      run(":settings -old-syntax:false")
    } andThen {
      storedOutput()
      run(":settings -old-syntax:true\nif true then println(\"REPL_SETTINGS_MARKER\")")
      val output = storedOutput()
      assertTrue(
        s"trailing code should be rejected under conflicting -old-syntax:true, got:\n$output",
        output.contains("This construct is not allowed under -old-syntax"))
      assertFalse(
        s"trailing code should not run, got:\n$output",
        output.linesIterator.map(_.trim).contains("REPL_SETTINGS_MARKER"))
    }

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

  @Test def `line comment before directive warns and evaluates code`: Unit =
    initially:
      run("// a comment\n//> using scala 3.3.1\nval x = 1")
      val output = storedOutput()
      assertTrue(output, output.contains("using scala"))
      assertTrue(output, output.contains("not supported in the REPL"))
      assertTrue(output, output.contains("val x: Int = 1"))

  @Test def `block comment before directive warns and evaluates code`: Unit =
    initially:
      run("/* block */\n//> using options -Werror\nval x = 1")
      val output = storedOutput()
      assertTrue(output, output.contains("using options"))
      assertTrue(output, output.contains("not supported in the REPL"))
      assertTrue(output, output.contains("val x: Int = 1"))

  @Test def `blank lines before directive warn and evaluate code`: Unit =
    initially:
      run("\n\n//> using options -Werror\nval x = 1")
      val output = storedOutput()
      assertTrue(output, output.contains("using options"))
      assertTrue(output, output.contains("not supported in the REPL"))
      assertTrue(output, output.contains("val x: Int = 1"))

  @Test def `comment between directives warns and evaluates code`: Unit =
    initially:
      run("//> using options -Werror\n// c\n//> using scala 3.3.1\nval z = 3")
      val output = storedOutput()
      assertTrue(output, output.contains("using options"))
      assertTrue(output, output.contains("using scala"))
      assertTrue(output, output.contains("not supported in the REPL"))
      assertTrue(output, output.contains("val z: Int = 3"))

  @Test def `line comment before command evaluates both`: Unit =
    initially:
      run("// c\n:type \"hi\"\nval x = 5")
      val output = storedOutput()
      assertTrue(output, output.contains("String"))
      assertTrue(output, output.contains("val x: Int = 5"))

  @Test def `block comment before command evaluates both`: Unit =
    initially:
      run("/* c */\n:type \"hi\"\nval x = 5")
      val output = storedOutput()
      assertTrue(output, output.contains("String"))
      assertTrue(output, output.contains("val x: Int = 5"))

  @Test def `comment between commands evaluates all`: Unit =
    initially:
      run(":type \"hi\"\n// c\n:type 42\nval y = 1")
      val output = storedOutput()
      assertTrue(output, output.contains("String"))
      assertTrue(output, output.contains("Int"))
      assertTrue(output, output.contains("val y: Int = 1"))

  @Test def `command after comment with incomplete code is incomplete`: Unit = contextually:
    assertTrue(ParseResult.isIncomplete("// c\n:settings -deprecation\nif true then"))
    assertFalse(ParseResult.shouldAcceptLine("// c\n:settings -deprecation\nif true then", hasPendingInput = false))

  @Test def `lone command after comment awaits trailing code`: Unit = contextually:
    assertTrue(ParseResult.onlyPreambleSoFar("// c\n:dep x"))
    assertTrue(ParseResult.shouldAcceptLine("// c\n:dep x\n", hasPendingInput = false))

  @Test def `lone directive after block comment awaits trailing code`: Unit = contextually:
    assertTrue(ParseResult.onlyPreambleSoFar("/* c */\n//> using dep x"))
    assertTrue(ParseResult.shouldAcceptLine("/* c */\n//> using dep x\n", hasPendingInput = false))

  @Test def `comment between commands parses as nested CommandThenCode`: Unit = initially:
    ParseResult(":type 1\n// c\n:type 2\nval x = 5") match
      case CommandThenCode(TypeOf("1"), rest) if rest.contains(":type 2") && rest.contains("val x = 5") => // expected
      case other => org.junit.Assert.fail(s"unexpected parse result: $other")

  @Test def `command then directive parses as mixed`: Unit = initially:
    ParseResult(":dep a::b:1\n//> using dep c::d:2\nval x = 1") match
      case MixedCommandsAndDirectives => // expected
      case other => org.junit.Assert.fail(s"unexpected parse result: $other")

  @Test def `directive then command parses as mixed`: Unit = initially:
    ParseResult("//> using dep c::d:2\n:dep a::b:1\nval x = 1") match
      case MixedCommandsAndDirectives => // expected
      case other => org.junit.Assert.fail(s"unexpected parse result: $other")

  @Test def `command then directive without trailing code parses as mixed`: Unit = initially:
    ParseResult(":dep a::b:1\n//> using dep c::d:2") match
      case MixedCommandsAndDirectives => // expected
      case other => org.junit.Assert.fail(s"unexpected parse result: $other")

  @Test def `multiple commands then directive parses as mixed`: Unit = initially:
    ParseResult(":type 1\n:type 2\n//> using dep c::d:2\nval x = 1") match
      case MixedCommandsAndDirectives => // expected
      case other => org.junit.Assert.fail(s"unexpected parse result: $other")

  @Test def `command then directive with interspersed comment parses as mixed`: Unit = initially:
    ParseResult(":dep a::b:1\n// c\n//> using dep c::d:2") match
      case MixedCommandsAndDirectives => // expected
      case other => org.junit.Assert.fail(s"unexpected parse result: $other")

  @Test def `directive then command with blank line parses as mixed`: Unit = initially:
    ParseResult("//> using dep c::d:2\n\n:dep a::b:1") match
      case MixedCommandsAndDirectives => // expected
      case other => org.junit.Assert.fail(s"unexpected parse result: $other")

  @Test def `mixing commands and directives is rejected without executing`: Unit =
    initially:
      run(":dep some.bogus::coords:1\n//> using dep other.bogus::coords:2\nval x = 1")
      val output = storedOutput()
      assertTrue(output, output.contains("Cannot mix"))
      assertFalse(output, output.contains("Resolved"))
      assertFalse(output, output.contains("val x: Int = 1"))

  @Test def `command with directive-like string is not mixed`: Unit = initially:
    ParseResult(":type 1\nval s = \"//> using dep x\"") match
      case CommandThenCode(TypeOf("1"), code) if code.contains("val s =") => // expected
      case other => org.junit.Assert.fail(s"unexpected parse result: $other")

  @Test def `block comment between commands parses as nested CommandThenCode`: Unit = initially:
    ParseResult(":type 1\n/* c */\n:type 2\nval x = 5") match
      case CommandThenCode(TypeOf("1"), rest) if rest.contains(":type 2") && rest.contains("val x = 5") => // expected
      case other => org.junit.Assert.fail(s"unexpected parse result: $other")

  @Test def `multiline block comment between commands parses as nested CommandThenCode`: Unit = initially:
    ParseResult(":type 1\n/* multi\n line\n comment */\n:type 2\nval x = 5") match
      case CommandThenCode(TypeOf("1"), rest) if rest.contains(":type 2") && rest.contains("val x = 5") => // expected
      case other => org.junit.Assert.fail(s"unexpected parse result: $other")

  @Test def `block comment between command and directive parses as mixed`: Unit = initially:
    ParseResult(":dep a::b:1\n/* c */\n//> using dep c::d:2") match
      case MixedCommandsAndDirectives => // expected
      case other => org.junit.Assert.fail(s"unexpected parse result: $other")

  @Test def `multiline block comment between command and directive parses as mixed`: Unit = initially:
    ParseResult(":dep a::b:1\n/* multi\n line */\n//> using dep c::d:2") match
      case MixedCommandsAndDirectives => // expected
      case other => org.junit.Assert.fail(s"unexpected parse result: $other")

  @Test def `mixed input is not incomplete`: Unit = contextually:
    assertFalse(ParseResult.isIncomplete(":dep a::b:1\n//> using dep c::d:2\nval x = 1"))
    assertFalse(ParseResult.isIncomplete("//> using dep c::d:2\n:dep a::b:1\nval x = 1"))

  @Test def `mixed input accepts line`: Unit = contextually:
    assertTrue(ParseResult.shouldAcceptLine(":dep a::b:1\n//> using dep c::d:2\nval x = 1", hasPendingInput = false))
    assertTrue(ParseResult.shouldAcceptLine("//> using dep c::d:2\n:dep a::b:1\nval x = 1", hasPendingInput = false))

  @Test def `directive first mixing is rejected without executing`: Unit =
    initially:
      run("//> using dep other.bogus::coords:2\n:dep some.bogus::coords:1\nval x = 1")
      val output = storedOutput()
      assertTrue(output, output.contains("Cannot mix"))
      assertFalse(output, output.contains("Resolved"))
      assertFalse(output, output.contains("val x: Int = 1"))

  @Test def `unsupported directive with command parses as mixed`: Unit = initially:
    ParseResult(":dep a::b:1\n//> using options -Werror") match
      case MixedCommandsAndDirectives => // expected
      case other => org.junit.Assert.fail(s"unexpected parse result: $other")

  @Test def `unsupported directive then command parses as mixed`: Unit = initially:
    ParseResult("//> using scala 3.3.1\n:type 1") match
      case MixedCommandsAndDirectives => // expected
      case other => org.junit.Assert.fail(s"unexpected parse result: $other")

  @Test def `post-code directive warning printed exactly once`: Unit =
    initially:
      run("val x = 1\n//> using dep foo::bar:1.0")
      val output = storedOutput()
      val count = "Ignoring using directive".r.findAllIn(output).length
      org.junit.Assert.assertEquals("warning should appear exactly once", 1, count)
      assertTrue(output, output.contains("val x: Int = 1"))

  @Test def `post-code directive diagnostic is surfaced`: Unit =
    initially:
      run("val y = 2\n//> using options -Werror")
      val output = storedOutput()
      assertTrue(output, output.contains("Ignoring using directive"))
      assertTrue(output, output.contains("val y: Int = 2"))

  @Test def `shebang with command then directive parses as mixed`: Unit = initially:
    ParseResult("#!/usr/bin/env scala\n:dep a::b:1\n//> using dep c::d:2") match
      case MixedCommandsAndDirectives => // expected
      case other => org.junit.Assert.fail(s"unexpected parse result: $other")

  @Test def `shebang with directive then command parses as mixed`: Unit = initially:
    ParseResult("#!/usr/bin/env scala\n//> using dep c::d:2\n:dep a::b:1") match
      case MixedCommandsAndDirectives => // expected
      case other => org.junit.Assert.fail(s"unexpected parse result: $other")

  @Test def `shebang with command only parses as command`: Unit = initially:
    ParseResult("#!/usr/bin/env scala\n:type 1\nval x = 5") match
      case CommandThenCode(TypeOf("1"), code) if code.contains("val x = 5") => // expected
      case other => org.junit.Assert.fail(s"unexpected parse result: $other")

  @Test def `shebang with directive only evaluates normally`: Unit = initially:
    ParseResult("#!/usr/bin/env scala\n//> using dep x::y:1\nval x = 1") match
      case _: Parsed => // expected
      case other => org.junit.Assert.fail(s"unexpected parse result: $other")
