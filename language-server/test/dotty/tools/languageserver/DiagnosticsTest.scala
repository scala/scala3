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

  @Test def diagnosticMissingLambdaBody: Unit =
    code"""object Test {
          |  Nil.map(x => x).filter(x$m1 =>$m2)
          |$m3}""".withSource
      .diagnostics(m1,
        (m2 to m3, "illegal start of simple expression", Error, Some(IllegalStartSimpleExprID)),
        (m1 to m1, """found:    Null
                     |required: Boolean""".stripMargin, Error, Some(TypeMismatchID))
      )
}
