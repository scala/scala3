package dotty.tools.dotc.printing

import dotty.tools.DottyTest
import org.junit.Assert._
import org.junit.{Ignore, Test}

/** Adapted from Ammonite HighlightTests
 */
class SyntaxHighlightingTests extends DottyTest {
  import SyntaxHighlighting._

  private def test(source: String, expected: String): Unit = {
    val highlighted = SyntaxHighlighting.highlight(source)(ctx)
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
  @Ignore("Comments are currently not supported")
  def comments = {
    test("//a", "<C|//a>")
    test("/** a */", "<C|/** a */>")
    test("/* a */", "<C|/* a */>")
  }

  @Test
  def types = {
    test("type Foo = Int", "<K|type> <T|Foo> = <T|Int>")
    test("type A = String | Int", "<K|type> <T|A> = <T|String | Int>")
    test("type B = String & Int", "<K|type> <T|B> = <T|String & Int>")
  }

  @Test
  def literals = {
    test("1", "<L|1>")
    test("1.1", "<L|1.1>")
    test("1.1.toString", "<L|1.1>.toString")
    // test("1L", "<L|1L>")
  }

  @Test
  def strings = {
    // For some reason we currently use literal color for string
    test("\"Hello\"", "<L|\"Hello\">")
    test("\"\"\"Hello\"\"\"", "<L|\"\"\"Hello\"\"\">")

    // FIXME: '$' should not be colored (literal position is off by one)
    // test("s\"Hello\"", "s<L|\"Hello\">")
    // test("s\"Hello $name\"", "s<L|\"Hello <V|$name<L|\">")
    // test("raw\"Hello\"", "raw<L|\"Hello\">")
    // test("raw\"\"\"Hello\"\"\"", "raw<L|\"\"\"Hello\"\"\">")
  }

  @Test
  def annotations = {
    test("@deprecated class Foo", "<T|@deprecated> <K|class> <T|Foo>")
    test("@Test() class Foo", "<T|@Test()> <K|class> <T|Foo>")
    test("@Test(\"Hello\") class Foo", "<T|@Test(\"Hello\")> <K|class> <T|Foo>")
    test("@Test(\"Hello\")(\"World\") class Foo", "<T|@Test(\"Hello\")(\"World\")> <K|class> <T|Foo>")
    test("@annotation.tailrec def foo = 1", "<T|@annotation.tailrec> <K|def> <V|foo> = <L|1>")
  }

  @Test
  def expressions = {
    test("if (true) 1 else 2", "<K|if> (<L|true>) <L|1> <K|else> <L|2>")
    test("val x = 1 + 2 + 3", "<K|val> <V|x> = <L|1> + <L|2> + <L|3>")
    test("if (true) 3 else 1", "<K|if> (<K|true>) <L|3> <K|else> <L|1>")
  }

  @Test
  def valOrDefDef = {
    test("val a = 123", "<K|val> <V|a> = <L|123>")
    test("var e: Int = 123", "<K|var> <V|e>: <T|Int> = <L|123>")
    test("def f = 123", "<K|def> <V|f> = <L|123>")
    test("def f1(x: Int) = 123", "<K|def> <V|f1>(<V|x>: <T|Int>) = <L|123>")
    test("def f2[T](x: T) = { 123 }", "<K|def> <V|f2>[<T|T>](<V|x>: <T|T>) = { <L|123> }")
  }

  @Test
  @Ignore("TODO: Not implemented")
  def patterns = {
    test("val Foo(x) = foo", ???)
    test("val foo @ Foo(x) = bar", ???)
    test("x match { case Foo | Bar => 1 }", ???)
  }
}
