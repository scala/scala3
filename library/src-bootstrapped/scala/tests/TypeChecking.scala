package scala.tests

import scala.quoted._
import scala.tasty.Reflection

object TypeChecking {

  inline def typeChecks(inline code: String): Boolean = ${ typeChecksImpl(code) }

  private def typeChecksImpl(code: String)(implicit reflect: Reflection): Expr[Boolean] = {
    import reflect._
    typing.typeChecks(code).toExpr
  }

}
