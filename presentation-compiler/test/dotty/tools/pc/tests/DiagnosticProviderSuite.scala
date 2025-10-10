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
import org.eclipse.lsp4j.Diagnostic
import org.eclipse.lsp4j.CodeAction

class DiagnosticProviderSuite extends PcAssertions {
  case class TestDiagnostic(startIndex: Int, endIndex: Int, msg: String, severity: DiagnosticSeverity)

  val pc = RawScalaPresentationCompiler().newInstance("", TestResources.classpath.asJava, Nil.asJava)

  def check(
    text: String,
    expected: List[TestDiagnostic],
    additionalChecks: List[Diagnostic] => Unit = identity
  ): Unit =
    val diagnostics = pc
      .didChange(CompilerVirtualFileParams(URI.create("file:/Diagnostic.scala"), text, EmptyCancelToken))
      .asScala

    val actual = diagnostics.map(d => TestDiagnostic(d.getRange().getStart().getOffset(text), d.getRange().getEnd().getOffset(text), d.getMessage(), d.getSeverity()))
    assertEquals(expected, actual, s"Expected [${expected.mkString(", ")}] but got [${actual.mkString(", ")}]")
    additionalChecks(diagnostics.toList)

  @Test def error =
    check(
      """|object M:
         |  Int.maaxValue
         |""".stripMargin,
      List(TestDiagnostic(12,25, "value maaxValue is not a member of object Int - did you mean Int.MaxValue?", DiagnosticSeverity.Error))
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
      """object M:
         |  Int.maaxValue
         |  1 + 1
         |""".stripMargin,
      List(
        TestDiagnostic(12,25, "value maaxValue is not a member of object Int - did you mean Int.MaxValue?", DiagnosticSeverity.Error),
        TestDiagnostic(28, 33, "A pure expression does nothing in statement position", DiagnosticSeverity.Warning)
      )
    )

  @Test def codeAction =
    check(
      """object M:
         |  private private class Test
         |""".stripMargin,
      List(
        TestDiagnostic(20, 27, "Repeated modifier private", DiagnosticSeverity.Error),
      ),
      diags =>
        val action = diags.head.getData().asInstanceOf[java.util.List[CodeAction]].asScala.head
        assertWithDiff("Remove repeated modifier: \"private\"", action.getTitle(), false)
        assertEquals(1, action.getEdit().getChanges().size(), "There should be one change")
    )

}
