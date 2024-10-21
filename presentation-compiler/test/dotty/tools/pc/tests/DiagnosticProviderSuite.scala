package dotty.tools.pc.tests

import scala.language.unsafeNulls

import dotty.tools.pc.base.BasePCSuite
import dotty.tools.pc.utils.RangeReplace

import java.net.URI
import org.eclipse.lsp4j.jsonrpc.messages.{Either => JEither}
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.internal.metals.EmptyCancelToken
import scala.meta.internal.pc.PcReferencesRequest

import org.junit.Test
import scala.collection.mutable.ListBuffer
import org.eclipse.lsp4j.DiagnosticSeverity
import scala.concurrent.duration.*
import dotty.tools.pc.utils.TestExtensions.getOffset

class DiagnosticProviderSuite extends BasePCSuite with RangeReplace {
  private val rangeRegex = "<<.*>>".r
  case class TestDiagnostic(startIndex: Int, endIndex: Int, msg: String, severity: DiagnosticSeverity)

  def check(
    text: String,
    expected: List[TestDiagnostic]
  ): Unit =
    val diagnostics = presentationCompiler
      .didChange(CompilerVirtualFileParams(URI.create("file:/Diagnostic.scala"), text, EmptyCancelToken))
      .get()
      .asScala

    val actual = diagnostics.map(d => TestDiagnostic(d.getRange().getStart().getOffset(text), d.getRange().getEnd().getOffset(text), d.getMessage(), d.getSeverity()))
    assertEquals(expected, actual, s"Expected [${expected.mkString(", ")}] but got [${actual.mkString(", ")}]")

  @Test def simple1 =
    check(
      """|class Bar(i: It)
         |""".stripMargin,
      List(TestDiagnostic(13, 15, "Not found: type It - did you mean Int.type? or perhaps Int?", DiagnosticSeverity.Error))
    )

  // @Test def `implicit-args-2` =
  //   check(
  //     """|package example
  //       |
  //       |class Bar(i: Int)
  //       |class Foo(implicit b: Bar)
  //       |
  //       |object Hello {
  //       |  implicit val ba@@rr: Bar = new Bar(1)
  //       |  val foo = new Foo<<>>
  //       |}
  //       |""".stripMargin
  //   )

  // @Test def `case-class` =
  //   check(
  //     """|case class Ma@@in(i: Int)
  //        |""".stripMargin
  //   )

  // @Test def `case-class-with-implicit` =
  //   check(
  //     """"|case class A()(implicit val fo@@o: Int)
  //         |""".stripMargin
  //   )
}
