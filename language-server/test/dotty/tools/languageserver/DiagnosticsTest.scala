package dotty.tools.languageserver

import org.junit.Test

import dotty.tools.dotc.reporting.diagnostic.ErrorMessageID._
import dotty.tools.languageserver.util.Code._
import org.eclipse.lsp4j.DiagnosticSeverity._

class DiagnosticsTest {
  @Test def diagnosticWrongType: Unit =
    code"""object Test {
          |  val x: Int = $m1"foo"$m2
          |}""".withSource
      .diagnostics(m1,
        (m1 to m2, """found:    String("foo")
                     |required: Int""".stripMargin, Error, Some(TypeMismatchID))
      )
}
