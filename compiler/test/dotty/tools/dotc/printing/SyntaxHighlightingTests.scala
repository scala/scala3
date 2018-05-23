package dotty.tools.dotc.printing

import dotty.tools.DottyTest
import org.junit.Assert._
import org.junit.Test

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
    test("1.1", "<L|1.1>")
    test("1.1.toString", "<L|1.1>.toString")
    // test("1L", "<L|1L>")
  }

  @Test
  def strings = {
    // For some reason we currently use literal color for string
    test("\"Hello\"", "<L|\"Hello\">")
    test("s\"Hello\"", "s<L|\"Hello\">")
    test("s\"Hello $name\"", "s<L|\"Hello <V|$name<L|\">")
    test("raw\"Hello\"", "raw<L|\"Hello\">")
    test("raw\"\"\"Hello\"\"\"", "raw<L|\"\"\"Hello\"\"\">")
  }

  @Test
  def annotations = {
    val source =
      """
        |@deprecated
        |class Foo {
        |  @inline val bar = 42
        |}
      """.stripMargin

    val expected =
      """
        |<T|@deprecated>
        |<K|class> <T|Foo> {
        |  <T|@inline> <K|val> <V|bar> = <L|42>
        |}
      """.stripMargin

    test(source, expected)

    test("@deprecated class Foo", "<T|@deprecated> <K|class> <T|Foo>")
  }

  @Test
  def expressions = {
    test("val x = 1 + 2 + 3", "<K|val> <V|x> = <L|1> + <L|2> + <L|3>")
    test("if (true) 3 else 1", "<K|if> (<K|true>) <L|3> <K|else> <L|1>")
  }

  @Test
  def valDef = {
    test("val a = 123", "<K|val> <V|a> = <L|123>")
    test("var b = 123 /*Int*/", "<K|var> <V|b> = <L|123> <C|/*Int*/>")
    test("""var c = "123" // String""", """<K|var> <V|c> = <L|"123"> <C|// String>""")
    test("var e:Int = 123;e", "<K|var> <V|e>:<T|Int> = <L|123>;e")
    test("def f = 123", "<K|def> <V|f> = <L|123>")
    test("def f1(x: Int) = 123", "<K|def> <V|f1>(x: <T|Int>) = <L|123>")
    test("def f2[T](x: T) = { 123 }", "<K|def> <V|f2>[<T|T>](x: <T|T>) = { <L|123> }")
  }

  @Test
  def patternMatching = {
    test("""val aFruit: Fruit = Apple("red", 123)""",
      """<K|val> <V|aFruit>: <T|Fruit> = <T|Apple>(<L|"red">, <L|123>)""")
    test("""val Apple(color, weight) = aFruit""",
      """<K|val> <T|Apple>(<V|color>, <V|weight>) = aFruit""")
    test("""case Apple(_, weight) => println(s"apple: $weight kgs")""",
      """<K|case> <T|Apple>(<V|_>, <V|weight>) <T|=>> println(s<L|"apple: <V|$weight <L|kgs">)""")
    test("""case o: Orange => println(s"orange ${o.weight} kgs")""",
      """<K|case> <V|o>: <T|Orange> <T|=>> println(s<L|"orange <V|${o.weight}<L| kgs">)""")
    test("""case m @ Melon(weight) => println(s"melon: ${m.weight} kgs")""",
      """<K|case> <V|m> @ <T|Melon>(<V|weight>) <T|=>> println(s<L|"melon: <V|${m.weight}<L| kgs">)""")
  }

  @Test
  def unionTypes = {
    test("type A = String|Int| Long", "<K|type> <T|A> = <T|String|Int| Long>")
    test("type B = String |Int| Long", "<K|type> <T|B> = <T|String |Int| Long>")
    test("type C = String | Int | Long", "<K|type> <T|C> = <T|String | Int | Long>")
    test("type D = String&Int& Long", "<K|type> <T|D> = <T|String&Int& Long>")
    test("type E = String &Int& Long", "<K|type> <T|E> = <T|String &Int& Long>")
    test("type F = String & Int & Long", "<K|type> <T|F> = <T|String & Int & Long>")
    test("fn[String|Char](input)", "fn[<T|String|Char>](input)")
  }
}
