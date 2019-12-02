import scala.quoted._

object api {
  inline def (inline x: String) stripMargin: String =
    ${ stripImpl(x) }

  private def stripImpl(x: String)(given qctx: QuoteContext): Expr[String] =
    Expr(augmentString(x).stripMargin)

  inline def typeChecks(inline x: String): Boolean =
    ${ typeChecksImpl(scala.compiletime.testing.typeChecks(x)) }

  private def typeChecksImpl(b: Boolean)(given qctx: QuoteContext): Expr[Boolean] = {
    if (b) Expr(true) else Expr(false)
  }
}
