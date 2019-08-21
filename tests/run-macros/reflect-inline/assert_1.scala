import scala.quoted._

object api {
  inline def (inline x: String) stripMargin: String =
    ${ stripImpl(x) }

  private def stripImpl(x: String) given (qctx: QuoteContext): Expr[String] =
    x.stripMargin.toExpr

  inline def typeChecks(inline x: String): Boolean =
    ${ typeChecksImpl(x) }

  private def typeChecksImpl(x: String) given (qctx: QuoteContext): Expr[Boolean] = {
    import qctx.tasty._
    if (qctx.tasty.typing.typeChecks(x)) true.toExpr else false.toExpr
  }
}
