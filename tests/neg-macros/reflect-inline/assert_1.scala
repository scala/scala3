import scala.quoted._

object api {
  inline def (inline x: String).stripMargin2: String =
    ${ stripImpl('x) }

  private def stripImpl(x: Expr[String]) with (qctx: QuoteContext) : Expr[String] =
    Expr(x.value.stripMargin)

}
