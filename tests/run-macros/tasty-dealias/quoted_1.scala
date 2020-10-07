import scala.quoted._

object Macros {

  inline def dealias[T]: String = ${ impl('[T]) }

  def impl[T](x: quoted.Type[T])(using qctx: QuoteContext) : Expr[String] = {
    import qctx.reflect._
    Expr(x.unseal.tpe.dealias.show)
  }
}
