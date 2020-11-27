package dotty.dokka.tasty.comments

import scala.quoted._

import org.junit.{Test, Rule}
import org.junit.Assert.{assertSame, assertTrue, assertEquals}
import dotty.dokka.tasty.util._
import dotty.dokka.tasty.TastyParser

class CommentExpanderTests {
  def check(using quoted.Quotes)(): Unit =
    assertCommentEquals(
      qr.Symbol.requiredClass("tests.B").method("otherMethod").head,
      "/** This is my foo: Bar, actually. */",
    )
    assertCommentEquals(
      qr.Symbol.requiredClass("tests.C"),
      "/** This is foo: Foo expanded. */",
    )
    assertCommentEquals(
      qr.Symbol.requiredModule("tests.O").method("method").head,
      "/** This is foo: O's foo. */",
    )


  def assertCommentEquals(
    using quoted.Quotes
  )(
    rsym: quotes.reflect.Symbol,
    str: String
  ): Unit =
    import dotty.tools.dotc
    given ctx as dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    val sym = rsym.asInstanceOf[dotc.core.Symbols.Symbol]
    val comment = CommentExpander.cookComment(sym).get
    assertEquals(comment.expanded.get, str)

  @Test
  def test(): Unit = {
    import scala.tasty.inspector.TastyInspector
    class Inspector extends TastyInspector:

      def processCompilationUnit(using quoted.Quotes)(root: quotes.reflect.Tree): Unit = ()

      override def postProcess(using quoted.Quotes): Unit =
        check()

    Inspector().inspectTastyFiles(TestUtils.listOurClasses())
  }

  private def qr(using quoted.Quotes): quotes.reflect.type = quotes.reflect
}
