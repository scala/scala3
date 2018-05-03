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

  @Test
  def valVarDef = {
    val source =
      """
        |package test
        |
        |object A {
        | val a = 123
        | var b = 123 /*Int*/
        | var c = "123" // String
        | var d: Int = 123
        | var e:Int = 123;e
        | e
        | print(a)
        | 123;123
        | def f = 123
        | def f1(x: Int) = 123
        | def f2[T](x: T) = { 123 }
        |}
      """.stripMargin
    val expected =
      """
        |<K|package> test
        |
        |<K|object> <T|A> {
        | <K|val> <V|a> = <L|123>
        | <K|var> <V|b> = <L|123> <C|/*Int*/>
        | <K|var> <V|c> = <L|"123"> <C|// String
        |> <K|var> <V|d>: <T|Int> = <L|123>
        | <K|var> <V|e>:<T|Int> = <L|123>;e
        | e
        | print(a)
        | <L|123>;<L|123>
        | <K|def> <V|f> = <L|123>
        | <K|def> <V|f1>(x: <T|Int>) = <L|123>
        | <K|def> <V|f2>[<T|T>](x: <T|T>) = { <L|123> }
        |}
      """.stripMargin
    test(source, expected)
  }

  @Test
  def patternMatching = {
    val source =
      """
        |val aFruit: Fruit = Apple("red", 123)
        |
        |val Apple(color, weight) = aFruit
        |
        |sealed trait Fruit
        |case class Apple(color: String, weight: Double) extends Fruit
        |case class Orange(weight: Double) extends Fruit
        |case class Melon(weight: Double) extends Fruit
        |
        |aFruit match {
        |  case Apple(_, weight) => println(s"apple: $weight kgs")
        |  case o: Orange => println(s"orange ${o.weight} kgs")
        |  case m @ Melon(weight) => println(s"melon: ${m.weight} kgs")
        |}
      """.stripMargin
    val expected =
      """
        |<K|val> <V|aFruit>: <T|Fruit> = <T|Apple>(<L|"red">, <L|123>)
        |
        |<K|val> <T|Apple>(<V|color>, <V|weight>) = aFruit
        |
        |<K|sealed> <K|trait> <T|Fruit>
        |<K|case> <K|class> <T|Apple>(color: <T|String>, weight: <T|Double>) <K|extends> <T|Fruit>
        |<K|case> <K|class> <T|Orange>(weight: <T|Double>) <K|extends> <T|Fruit>
        |<K|case> <K|class> <T|Melon>(weight: <T|Double>) <K|extends> <T|Fruit>
        |
        |aFruit <K|match> {
        |  <K|case> <T|Apple>(<V|_>, <V|weight>) <T|=>> println(s<L|"apple: <V|$weight <L|kgs">)
        |  <K|case> <V|o>: <T|Orange> <T|=>> println(s<L|"orange <V|${o.weight}<L| kgs">)
        |  <K|case> <V|m> @ <T|Melon>(<V|weight>) <T|=>> println(s<L|"melon: <V|${m.weight}<L| kgs">)
        |}
      """.stripMargin
    test(source, expected)
  }

  @Test
  def unionTypes = {
    val source =
      """
        |type A = String|Int| Long
        |type B = String |Int| Long
        |type C = String | Int | Long
        |type D = String&Int& Long
        |type E = String &Int& Long
        |type F = String & Int & Long
        |fn[String|Char](input)
      """.stripMargin
    val expected =
      """
        |<K|type> <T|A> = <T|String>|<T|Int>| <T|Long>
        |<K|type> <T|B> = <T|String> |<T|Int>| <T|Long>
        |<K|type> <T|C> = <T|String> | <T|Int> | <T|Long>
        |<K|type> <T|D> = <T|String>&<T|Int>& <T|Long>
        |<K|type> <T|E> = <T|String> &<T|Int>& <T|Long>
        |<K|type> <T|F> = <T|String> & <T|Int> & <T|Long>
        |fn[<T|String>|<T|Char>](input)
      """.stripMargin
    test(source, expected)
  }
}
