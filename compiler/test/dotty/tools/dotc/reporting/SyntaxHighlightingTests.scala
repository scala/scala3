package dotty.tools.dotc.printing

import org.junit.Assert._
import org.junit.Test

/** Adapted from Ammonite HighlightTests
 */
class SyntaxHighlightingTests {
  import SyntaxHighlighting._

  private def test(source: String, expected: String): Unit = {
    val highlighted = SyntaxHighlighting.apply(source)
      .mkString
      .replace(NoColor,         ">")
      .replace(CommentColor,    "<C|")
      .replace(KeywordColor,    "<K|")
      .replace(ValDefColor,     "<V|")
      .replace(LiteralColor,    "<L|")
      .replace(StringColor,     "<S|")
      .replace(TypeColor,       "<T|")
      // .replace(AnnotationColor, "<A|") // is the same color as type color

    if (expected != highlighted) {
      // assertEquals produces weird expected/found message
      fail(s"expected: $expected but was: $highlighted")
    }
  }

  @Test
  def comments = {
    test("//a", "<C|//a>")
    test("/** a */", "<C|/** a */>")
    test("/* a */", "<C|/* a */>")
  }

  @Test
  def types = {
    test("type Foo = Int", "<K|type> <T|Foo> = <T|Int>")
  }

  @Test
  def literals = {
    test("1", "<L|1>")
    // test("1L", "<L|1L>")
  }

  @Test
  def strings = {
    // For some reason we currently use literal color for string
    test("\"Hello\"", "<L|\"Hello\">")
  }

  @Test
  def annotations = {
    test("@tailrec", "<T|@tailrec>")
  }

  @Test
  def expressions = {
    test("val x = 1 + 2 + 3", "<K|val> <V|x> = <L|1> + <L|2> + <L|3>")
  }
}
