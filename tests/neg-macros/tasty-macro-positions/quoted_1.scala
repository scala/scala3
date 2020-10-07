import scala.quoted._

object Macros {

  inline def fun(x: Any): Unit = ${ impl('x) }

  def impl(x: Expr[Any])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.reflect._
    val pos = x.unseal.underlyingArgument.pos
    Reporting.error("here is the the argument is " + x.unseal.underlyingArgument.show, pos)
    Reporting.error("here (+5) is the the argument is " + x.unseal.underlyingArgument.show, pos.sourceFile, pos.start + 5, pos.end + 5)
    '{}
  }

}
