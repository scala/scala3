
package dotty.tools.dotc.config

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

class CommandLineParserTest:
  import CommandLineParser.{tokenize, ParseException}

  private def check(tokens: String*)(input: String): Unit = assertEquals(tokens, tokenize(input))

  private def checkFails(input: String): Unit =
    var failed = false
    val res = tokenize(input, _ => failed = true)
    assertTrue(s"Expected bad tokenization for [$input] but result was [$res]", failed)

  @Test def parserTokenizes() =
    check()("")
    check("x")("x")
    check("x", "y")("x y")
    check("x", "y", "z")("x y z")

  @Test def parserTrims() =
    check()(" ")
    check("x")(" x ")
    check("x")("\nx\n")
    check("x", "y", "z")(" x y z ")

  @Test def parserQuotes() =
    check("x")("'x'")
    check("x")(""""x"""")
    check("x", "y", "z")("x 'y' z")
    check("x", " y ", "z")("x ' y ' z")
    check("x", "y", "z")("""x "y" z""")
    check("x", " y ", "z")("""x " y " z""")
    // interior quotes
    check("x y z")("x' y 'z")   // was assertEquals(List("x'","y","'z"), tokenize("x' y 'z"))
    check("x\ny\nz")("x'\ny\n'z")
    check("x'y'z")("""x"'y'"z""")
    check("abcxyz")(""""abc"xyz""")
    // missing quotes
    checkFails(""""x""")         // was assertEquals(List("\"x"), tokenize(""""x"""))
    checkFails("""x'""")
