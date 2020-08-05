import scala.quoted._

object Macros {

  inline def fun(x: Any): Unit = ${ impl('x) }

  def impl(x: Expr[Any])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.tasty._
    error("here is the the argument is " + x.asTerm.underlyingArgument.show, x.asTerm.underlyingArgument.pos)
    '{}
  }

}
