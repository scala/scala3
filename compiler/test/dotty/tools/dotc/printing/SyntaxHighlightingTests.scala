package dotty.tools.dotc.printing

import scala.language.unsafeNulls

import dotty.tools.DottyTest
import org.junit.Assert._
import org.junit.{Ignore, Test}

/** Adapted from Ammonite HighlightTests
 */
class SyntaxHighlightingTests extends DottyTest {
  import SyntaxHighlighting._

  private def test(source: String, expected: String): Unit = {
    val highlighted = SyntaxHighlighting.highlight(source)(using ctx.withColors)
      .replace(NoColor,         ">")
      .replace(CommentColor,    "<C|")
      .replace(KeywordColor,    "<K|")
      .replace(DefinitionColor, "<D|")
      .replace(LiteralColor,    "<L|")
      // .replace(TypeColor,       "<L|") Same as LiteralColor

    if (expected != highlighted) {
      // assertEquals produces weird expected/found message
      fail(s"""|
               |expected:      $expected
               |highlighted:   $highlighted""".stripMargin)
    }
  }

  @Test
  def comments = {
    test("// a", "<C|// a>")
    test("/** a */", "<C|/** a */>")
    test("/* a */", "<C|/* a */>")
  }

  @Test
  def types = {
    test("type Foo", "<K|type> <D|Foo>")
    test("type Foo =", "<K|type> <D|Foo> =")
    test("type Foo = Int", "<K|type> <D|Foo> = <L|Int>")
    test("type A = String | Int", "<K|type> <D|A> = <L|String> <L||> <L|Int>")
    test("type B = String & Int", "<K|type> <D|B> = <L|String> <L|&> <L|Int>")
    test("type Id[A] = A", "<K|type> <D|Id>[<D|A>] = <L|A>")
    test("type Foo = [X] =>> List[X]", "<K|type> <D|Foo> = [<D|X>] =>> <L|List>[<L|X>]")
  }

  @Test
  def literals = {
    test("1", "<L|1>")
    test("1.1", "<L|1.1>")
    test("1.1.toString", "<L|1.1>.toString")
    test("1L", "<L|1L>")
    test("1Lx", "<L|1L>x")
    test("1f", "<L|1f>")
    test("1.1f", "<L|1.1f>")
    test("1.1fx", "1.1fx")
  }

  @Test
  def strings = {
    // For some reason we currently use literal color for string
    test("\"Hello\"", "<L|\"Hello\">")
    test("\"\"\"Hello\"\"\"", "<L|\"\"\"Hello\"\"\">")

    test("s\"Hello\"", "<L|s\"Hello\">")
    test("s\"Hello $name\"", "<L|s\"Hello >$name<L|\">")
    test("s\"Hello ${name}\"", "<L|s\"Hello >${name}<L|\">")
    test("raw\"Hello\"", "<L|raw\"Hello\">")
  }

  @Test
  def annotations = {
    test("@deprecated class Foo", "<L|@deprecated> <K|class> <D|Foo>")
    test("@Test() class Foo", "<L|@Test()> <K|class> <D|Foo>")
    test("@Test(\"Hello\") class Foo", "<L|@Test(\"Hello\")> <K|class> <D|Foo>")
    test("@Test(\"Hello\")(\"World\") class Foo", "<L|@Test(\"Hello\")(\"World\")> <K|class> <D|Foo>")
    test("@annotation.tailrec def foo = 1", "<L|@annotation.tailrec> <K|def> <D|foo> = <L|1>")
  }

  @Test
  def expressions = {
    test("if (true) 1 else 2", "<K|if> (<L|true>) <L|1> <K|else> <L|2>")
    test("1 + 2 + 3", "<L|1> + <L|2> + <L|3>")
  }

  @Test
  def valOrDefDef = {
    test("val",           "<K|val>")
    test("val foo",       "<K|val> <D|foo>")
    test("val foo =",     "<K|val> <D|foo> =")
    test("val foo = 123", "<K|val> <D|foo> = <L|123>")
    test(
      "val foo: List[List[Int]] = List(List(1))",
      "<K|val> <D|foo>: <L|List>[<L|List>[<L|Int>]] = <K|List>(<K|List>(<L|1>))"
    )

    test("var",                "<K|var>")
    test("var foo",            "<K|var> <D|foo>")
    test("var foo:",           "<K|var> <D|foo>:")
    test("var foo: Int",       "<K|var> <D|foo>: <L|Int>")
    test("var foo: Int =",     "<K|var> <D|foo>: <L|Int> =")
    test("var foo: Int = 123", "<K|var> <D|foo>: <L|Int> = <L|123>")

    test("def",                          "<K|def>")
    test("def foo",                      "<K|def> <D|foo>")
    test("def foo(",                     "<K|def> <D|foo>(")
    test("def foo(bar",                  "<K|def> <D|foo>(<D|bar>")
    test("def foo(bar:",                 "<K|def> <D|foo>(<D|bar>:")
    test("def foo(bar: Int",             "<K|def> <D|foo>(<D|bar>: <L|Int>")
    test("def foo(bar: Int)",            "<K|def> <D|foo>(<D|bar>: <L|Int>)")
    test("def foo(bar: Int):",           "<K|def> <D|foo>(<D|bar>: <L|Int>):")
    test("def foo(bar: Int): Int",       "<K|def> <D|foo>(<D|bar>: <L|Int>): <L|Int>")
    test("def foo(bar: Int): Int =",     "<K|def> <D|foo>(<D|bar>: <L|Int>): <L|Int> =")
    test("def foo(bar: Int): Int = 123", "<K|def> <D|foo>(<D|bar>: <L|Int>): <L|Int> = <L|123>")

    test("def f1(x: Int) = 123", "<K|def> <D|f1>(<D|x>: <L|Int>) = <L|123>")
    test("def f2[T](x: T) = { 123 }", "<K|def> <D|f2>[<D|T>](<D|x>: <L|T>) = { <L|123> }")

    test("def f3[T[_", "<K|def> <D|f3>[<D|T>[_")
  }

  @Test
  @Ignore("TODO: Not implemented")
  def patterns = {
    test("val Foo(x) = foo", ???)
    test("val foo @ Foo(x) = bar", ???)
    test("x match { case Foo | Bar => 1 }", ???)
  }

  @Test
  def softKeywords = {
    test("inline def foo = 1", "<K|inline> <K|def> <D|foo> = <L|1>")
    test("@inline def foo = 1", "<L|@inline> <K|def> <D|foo> = <L|1>")
    test("class inline", "<K|class> <D|inline>")
    test("val inline = 2", "<K|val> <D|inline> = <L|2>")
    test("def inline = 2", "<K|def> <D|inline> = <L|2>")
    test("def foo(inline: Int) = 2", "<K|def> <D|foo>(<D|inline>: <L|Int>) = <L|2>")
    test(
      """enum Foo:
        |  case foo
        |end Foo""".stripMargin,
      """<K|enum> <D|Foo>:
        |  <K|case> <D|foo>
        |<K|end> <D|Foo>""".stripMargin
    )
    test(
      """class Foo:
        |end Foo""".stripMargin,
      """<K|class> <D|Foo>:
        |<K|end> <D|Foo>""".stripMargin
    )
    test(
      """object Foo:
        |end Foo""".stripMargin,
      """<K|object> <D|Foo>:
        |<K|end> <D|Foo>""".stripMargin
    )
    test(
      """def foo =
        |  ()
        |end foo""".stripMargin,
      """<K|def> <D|foo> =
        |  ()
        |<K|end> <D|foo>""".stripMargin
    )
    test(
      """val foo =
        |  ()
        |end foo""".stripMargin,
      """<K|val> <D|foo> =
        |  ()
        |<K|end> <D|foo>""".stripMargin
    )
  }
}
