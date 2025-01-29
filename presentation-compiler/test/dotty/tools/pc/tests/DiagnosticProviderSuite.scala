package dotty.tools.pc.tests

import scala.language.unsafeNulls

import dotty.tools.pc.base.BasePCSuite
import dotty.tools.pc.utils.RangeReplace

import java.net.URI
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.internal.metals.EmptyCancelToken

import org.junit.Test
import org.eclipse.lsp4j.DiagnosticSeverity
import dotty.tools.pc.utils.TestExtensions.getOffset

class DiagnosticProviderSuite extends BasePCSuite with RangeReplace {
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

  @Test def error =
    check(
      """|class Bar(i: It)
         |""".stripMargin,
      List(TestDiagnostic(13, 15, "Not found: type It - did you mean Int.type? or perhaps Int?", DiagnosticSeverity.Error))
    )

  @Test def warning =
    check(
      """|object M:
         |  1 + 1
         |""".stripMargin,
      List(TestDiagnostic(12, 17, "A pure expression does nothing in statement position", DiagnosticSeverity.Warning))
    )

  @Test def mixed =
    check(
      """|class Bar(i: It)
         |object M:
         |  1 + 1
         |""".stripMargin,
      List(
        TestDiagnostic(13 ,15, "Not found: type It - did you mean Int.type? or perhaps Int?", DiagnosticSeverity.Error),
        TestDiagnostic(29, 34, "A pure expression does nothing in statement position", DiagnosticSeverity.Warning)
      )
    )

}
