import scala.quoted._

object api {
  inline def (inline x: String) stripMargin: String =
    ${ stripImpl('x) }

  private def stripImpl(x: Expr[String])(using qctx: QuoteContext): Expr[String] =
    Lifted(augmentString(x.value).stripMargin)

  inline def typeChecks(inline x: String): Boolean =
    ${ typeChecksImpl('{scala.compiletime.testing.typeChecks(x)}) }

  private def typeChecksImpl(b: Expr[Boolean])(using qctx: QuoteContext): Expr[Boolean] = {
    if (b.value) Lifted(true) else Lifted(false)
  }
}
