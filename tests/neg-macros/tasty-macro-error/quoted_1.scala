import scala.quoted._

object Macros {

  inline def fun(x: Any): Unit = ${ impl('x) }

  def impl(x: Expr[Any]) with (qctx: QuoteContext) : Expr[Unit] = {
    import qctx.tasty.{_, given _}
    error("here is the the argument is " + x.unseal.underlyingArgument.show, x.unseal.underlyingArgument.pos)
    '{}
  }

}
