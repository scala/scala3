package dotty.tools.scaladoc
package tasty
package comments

import scala.quoted._

import org.junit.{Test, Rule}
import org.junit.Assert.{assertSame, assertTrue, assertEquals}
import dotty.tools.scaladoc.tasty.util._
import dotty.tools.scaladoc.tasty.TastyParser

class CommentExpanderTests {
  def check(using Quotes)(): Unit =
    assertCommentEquals(
      reflect.Symbol.requiredClass("tests.B").methodMember("otherMethod").head,
      "/** This is my foo: Bar, actually. */",
    )
    assertCommentEquals(
      reflect.Symbol.requiredClass("tests.C"),
      "/** This is foo: Foo expanded. */",
    )
    assertCommentEquals(
      reflect.Symbol.requiredModule("tests.O").methodMember("method").head,
      "/** This is foo: O's foo. */",
    )


  def assertCommentEquals(
    using Quotes
  )(
    rsym: reflect.Symbol,
    str: String
  ): Unit =
    import dotty.tools.dotc
    given ctx: dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    val sym = rsym.asInstanceOf[dotc.core.Symbols.Symbol]
    val comment = CommentExpander.cookComment(sym).get
    assertEquals(comment.expanded.get, str)

  @Test
  def test(): Unit = {
    import scala.tasty.inspector.OldTastyInspector
    class Inspector extends OldTastyInspector:

      def processCompilationUnit(using Quotes)(root: reflect.Tree): Unit = ()

      override def postProcess(using Quotes): Unit =
        check()

    Inspector().inspectTastyFiles(TestUtils.listOurClasses())
  }

}
