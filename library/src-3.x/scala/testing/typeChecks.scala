package scala.testing

import scala.quoted._
import scala.tasty.Reflection

inline def typeChecks(inline code: String): Boolean = ${ typeChecksImpl(code) }

private def typeChecksImpl(code: String) given StagingContext: Expr[Boolean] = {
  val reflect = the[StagingContext].reflection
  import reflect._
  typing.typeChecks(code).toExpr
}

