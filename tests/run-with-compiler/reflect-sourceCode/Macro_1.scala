import scala.quoted._

object api {
  inline def (x: => T) reflect[T] : String =
    ${ reflImpl('x) }

  private def reflImpl[T](x: Expr[T])(implicit qctx: QuoteContext): Expr[String] = {
    import qctx.tasty._
    x.unseal.pos.sourceCode.toExpr
  }
}
