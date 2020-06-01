import scala.quoted._

object api {
  inline def [T](x: => T).reflect: String =
    ${ reflImpl('x) }

  private def reflImpl[T](x: Expr[T])(implicit qctx: QuoteContext): Expr[String] = {
    import qctx.tasty._
    Expr(x.unseal.pos.sourceCode)
  }
}
