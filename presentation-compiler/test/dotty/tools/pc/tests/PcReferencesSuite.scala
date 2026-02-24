package dotty.tools.pc.tests

import java.net.URI

import scala.language.unsafeNulls
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.internal.metals.EmptyCancelToken
import scala.meta.internal.pc.PcReferencesRequest

import dotty.tools.pc.base.BasePCSuite
import dotty.tools.pc.utils.RangeReplace

import org.eclipse.lsp4j.jsonrpc.messages.Either as JEither
import org.junit.Test

class PcReferencesSuite extends BasePCSuite with RangeReplace {
  def check(
      original: String
  ): Unit =
    val edit = original.replaceAll("(<<|>>)", "")
    val expected = original.replaceAll("@@", "")
    val base = original.replaceAll("(<<|>>|@@)", "")

    val (code, offset) = params(edit, "Highlight.scala")
    val ranges = presentationCompiler
      .references(
        PcReferencesRequest(
          CompilerVirtualFileParams(
            URI.create("file:/Highlight.scala"),
            code,
            EmptyCancelToken
          ),
          includeDefinition = false,
          offsetOrSymbol = JEither.forLeft(offset)
        )
      )
      .get()
      .asScala
      .flatMap(_.locations().asScala.map(_.getRange()))
      .toList

    assertEquals(
      renderRangesAsString(base, ranges),
      expected,
      "references should match"
    )

  @Test def `implicit-args` =
    check(
      """|package example
         |
         |class Bar(i: Int)
         |
         |object Hello {
         |  def m(i: Int)(implicit b: Bar) = ???
         |  val foo = {
         |    implicit val ba@@rr: Bar = new Bar(1)
         |    m(3)<<>>
         |  }
         |}
         |""".stripMargin
    )

  @Test def `implicit-args-2` =
    check(
      """|package example
        |
        |class Bar(i: Int)
        |class Foo(implicit b: Bar)
        |
        |object Hello {
        |  implicit val ba@@rr: Bar = new Bar(1)
        |  val foo = new Foo<<>>
        |}
        |""".stripMargin
    )

  @Test def `case-class` =
    check(
      """|case class Ma@@in(i: Int)
         |""".stripMargin
    )

  @Test def `case-class-with-implicit` =
    check(
      """"|case class A()(implicit val fo@@o: Int)
          |""".stripMargin
    )
}
