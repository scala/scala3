package dotty.tools.pc.tests

import java.net.URI
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.internal.metals.EmptyCancelToken

import org.junit.Test
import org.eclipse.lsp4j.DiagnosticSeverity
import dotty.tools.pc.utils.TestExtensions.getOffset
import dotty.tools.pc.base.TestResources
import java.nio.file.Path
import dotty.tools.pc.RawScalaPresentationCompiler
import dotty.tools.pc.utils.PcAssertions

class DiagnosticProviderSuite extends PcAssertions {
  case class TestDiagnostic(startIndex: Int, endIndex: Int, msg: String, severity: DiagnosticSeverity)

  val pc = RawScalaPresentationCompiler().newInstance("", TestResources.classpath.asJava, Nil.asJava)

  def check(
    text: String,
    expected: List[TestDiagnostic]
  ): Unit =
    val diagnostics = pc
      .didChange(CompilerVirtualFileParams(URI.create("file:/Diagnostic.scala"), text, EmptyCancelToken))
      .asScala

    val actual = diagnostics.map(d => TestDiagnostic(d.getRange().getStart().getOffset(text), d.getRange().getEnd().getOffset(text), d.getMessage(), d.getSeverity()))
    // NOTE IF those tests turn out to be flaky, consider just checking the beginning of the message
    assertEquals(expected, actual, s"Expected [${expected.mkString(", ")}] but got [${actual.mkString(", ")}]")

  @Test def error =
    check(
      """|class Bar(i: It)
         |""".stripMargin,
      List(TestDiagnostic(13, 15, "Not found: type It - did you mean Int? or perhaps Int.type?", DiagnosticSeverity.Error))
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
        TestDiagnostic(13 ,15, "Not found: type It - did you mean Int? or perhaps Int.type?", DiagnosticSeverity.Error),
        TestDiagnostic(29, 34, "A pure expression does nothing in statement position", DiagnosticSeverity.Warning)
      )
    )

}
