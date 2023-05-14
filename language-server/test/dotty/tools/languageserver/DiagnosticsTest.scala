package dotty.tools.languageserver

import org.junit.Test

import dotty.tools.dotc.reporting.ErrorMessageID._
import dotty.tools.languageserver.util.Code._
import org.eclipse.lsp4j.DiagnosticSeverity._

class DiagnosticsTest {
  @Test def diagnosticWrongType: Unit =
    code"""object Test {
          |  val x: Int = $m1"foo"$m2
          |}"""
      .diagnostics(m1,
        (m1 to m2, """Found:    ("foo" : String)
                     |Required: Int""".stripMargin, Error, Some(TypeMismatchID))
      )

  @Test def diagnosticMissingLambdaBody: Unit =
    code"""object Test {
          |  Nil.map(x => x).filter(x$m1 =>$m2)$m3
          |}"""
      .diagnostics(m1,
        (m2 to m2, "expression expected but ')' found", Error, Some(IllegalStartSimpleExprID))
      )

  @Test def diagnosticPureExpression: Unit =
    code"""object Test {
          |  ${m1}1$m2
          |}"""
     .diagnostics(m1,
       (m1 to m2,
        "A pure expression does nothing in statement position; you may be omitting necessary parentheses",
        Warning, Some(PureExpressionInStatementPositionID)))

  @Test def diagnosticWorksheetPureExpression: Unit =
    ws"""${m1}1"""
     .diagnostics(m1 /* no "pure expression" warning because this is a worksheet */)
}
