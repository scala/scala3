package dotty.tools.pc.tests

import java.net.URI

import scala.meta.internal.jdk.CollectionConverters.*

import dotty.tools.pc.base.BaseDiagnosticsSuite

import org.eclipse.lsp4j.CodeAction
import org.eclipse.lsp4j.Diagnostic
import org.eclipse.lsp4j.DiagnosticSeverity
import org.junit.Test

class DiagnosticProviderSuite extends BaseDiagnosticsSuite {

  @Test def error =
    check(
      """|object M:
         |  Int.maaxValue
         |""".stripMargin,
      List(
        TestDiagnostic(
          12,
          25,
          "value maaxValue is not a member of object Int - did you mean Int.MaxValue?",
          DiagnosticSeverity.Error
        )
      )
    )

  @Test def warning =
    check(
      """|object M:
         |  1 + 1
         |""".stripMargin,
      List(
        TestDiagnostic(
          12,
          17,
          "A pure expression does nothing in statement position",
          DiagnosticSeverity.Warning
        )
      )
    )

  @Test def mixed =
    check(
      """object M:
         |  Int.maaxValue
         |  1 + 1
         |""".stripMargin,
      List(
        TestDiagnostic(
          12,
          25,
          "value maaxValue is not a member of object Int - did you mean Int.MaxValue?",
          DiagnosticSeverity.Error
        ),
        TestDiagnostic(
          28,
          33,
          "A pure expression does nothing in statement position",
          DiagnosticSeverity.Warning
        )
      )
    )

  @Test def codeAction =
    check(
      """object M:
         |  private private class Test
         |""".stripMargin,
      List(
        TestDiagnostic(
          20,
          27,
          "Repeated modifier private",
          DiagnosticSeverity.Error
        )
      ),
      diags =>
        val action = diags.head
          .getData()
          .asInstanceOf[java.util.List[CodeAction]]
          .asScala
          .head
        assertWithDiff(
          "Remove repeated modifier: \"private\"",
          action.getTitle(),
          false
        )
        assertEquals(
          1,
          action.getEdit().getChanges().size(),
          "There should be one change"
        )
    )
}
