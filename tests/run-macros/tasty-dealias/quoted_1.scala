import scala.quoted._

object Macros {

  inline def dealias[T]: String = ${ impl('[T]) }

  def impl[T](x: quoted.Staged[T])(using qctx: QuoteContext) : Expr[String] = {
    import qctx.tasty._
    Expr(x.unseal.tpe.dealias.show)
  }
}
