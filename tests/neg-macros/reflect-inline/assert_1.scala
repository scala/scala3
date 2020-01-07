import scala.quoted._

object api {
  inline def (inline x: String).stripMargin2: String =
    ${ stripImpl(x) }

  private def stripImpl(x: String)(given qctx: QuoteContext): Expr[String] =
    Expr(x.stripMargin)

}
