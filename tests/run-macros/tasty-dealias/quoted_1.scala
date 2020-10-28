import scala.quoted._

object Macros {

  inline def dealias[T]: String = ${ impl(Type[T]) }

  def impl[T](x: Type[T])(using qctx: QuoteContext) : Expr[String] = {
    import qctx.reflect._
    Expr(x.unseal.tpe.dealias.show)
  }
}
