
package dotty.tools.dotc.config

import scala.language.unsafeNulls

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

class CommandLineParserTest:
  import CommandLineParser.tokenize

  private def check(tokens: String*)(input: String): Unit = assertEquals(tokens, tokenize(input))

  private def checkFails(input: String, output: String): Unit =
    var txt: String = null
    val res = tokenize(input, msg => txt = msg)
    assertTrue(s"Expected bad tokenization for [$input] but result was [$res]", txt ne null)
    assertEquals(output, txt)

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
    checkFails(""""x""", "Unmatched quote [0](\")")  // was assertEquals(List("\"x"), tokenize(""""x"""))
    checkFails("""x'""", "Unmatched quote [1](')")

  @Test def `leading quote is escaped`: Unit =
    check("echo", "hello, world!")("""echo "hello, world!" """)
    check("echo", "hello, world!")("""echo hello,' 'world! """)
    check("echo", """\"hello,""", """world!\"""")("""echo \"hello, world!\" """)
    check("""a\"b\"c""")("""a\"b\"c""")
    check("a", "\\'b", "\\'", "c")("""a \'b \' c""")
    check("a", "\\\\b ", "c")("""a \\'b ' c""")
