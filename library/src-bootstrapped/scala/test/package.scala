package scala

import scala.quoted._
import scala.tasty.Reflection

package object test {

  inline def typeChecks(inline code: String): Boolean = ${ typeChecksImpl(code) }

  private def typeChecksImpl(code: String)(implicit reflect: Reflection): Expr[Boolean] = {
    import reflect._
    typing.typeChecks(code).toExpr
  }

}
