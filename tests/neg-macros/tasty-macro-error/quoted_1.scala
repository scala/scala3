import scala.quoted._

object Macros {

  inline def fun(x: Any): Unit = ${ impl('x) }

  def impl(x: Expr[Any])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.reflect._
    Reporting.error("here is the the argument is " + x.asReflectTree.underlyingArgument.show, x.asReflectTree.underlyingArgument.pos)
    '{}
  }

}
