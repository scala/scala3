import scala.quoted._

object Macros {

  inline def fun(x: Any): Unit = ${ impl('x) }

  def impl(x: Expr[Any])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.tasty._
    val pos = x.asTerm.underlyingArgument.pos
    error("here is the the argument is " + x.asTerm.underlyingArgument.show, pos)
    error("here (+5) is the the argument is " + x.asTerm.underlyingArgument.show, pos.sourceFile, pos.start + 5, pos.end + 5)
    '{}
  }

}
