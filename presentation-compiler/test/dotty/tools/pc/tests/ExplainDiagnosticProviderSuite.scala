package dotty.tools.pc.tests

import java.net.URI

import scala.meta.internal.jdk.CollectionConverters.*

import dotty.tools.pc.base.BaseDiagnosticsSuite

import org.eclipse.lsp4j.CodeAction
import org.eclipse.lsp4j.Diagnostic
import org.eclipse.lsp4j.DiagnosticSeverity
import org.junit.Test

class ExplainDiagnosticProviderSuite extends BaseDiagnosticsSuite {

  override def options: List[String] = List("-explain")

  @Test def error1 =
    check(
      """|object C:
         |  def m(x: Int) = 1
         |  object T extends K:
         |    val x = m(1) // error
         |class K:
         |  def m(i: Int) = 2
         |""".stripMargin,
      List(
        TestDiagnostic(
          64,
          65,
          """|Reference to m is ambiguous.
           |It is both defined in object C
           |and inherited subsequently in object T
           |
           |# Explanation (enabled by `-explain`)
           |
           |The identifier m is ambiguous because a name binding of lower precedence
           |in an inner scope cannot shadow a binding with higher precedence in
           |an outer scope.
           |
           |The precedence of the different kinds of name bindings, from highest to lowest, is:
           | - Definitions in an enclosing scope
           | - Inherited definitions and top-level definitions in packages
           | - Names introduced by import of a specific name
           | - Names introduced by wildcard import
           | - Definitions from packages in other files
           |Note:
           | - As a rule, definitions take precedence over imports.
           | - Definitions in an enclosing scope take precedence over inherited definitions,
           |   which can result in ambiguities in nested classes.
           | - When importing, you can avoid naming conflicts by renaming:
           |   import scala.{m => mTick}
           |""".stripMargin,
          DiagnosticSeverity.Error
        )
      )
    )
}
