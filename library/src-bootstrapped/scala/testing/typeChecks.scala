package scala.testing

import scala.quoted._

inline def typeChecks(inline code: String): Boolean = ${ typeChecksImpl(code) }

private def typeChecksImpl(code: String) given (qctx: QuoteContext): Expr[Boolean] = {
  import qctx.tasty._
  typing.typeChecks(code).toExpr
}
