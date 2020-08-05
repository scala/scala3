import scala.quoted._

object Macros {

  inline def dealias[T]: String = ${ impl('[T]) }

  def impl[T](x: quoted.Type[T])(using qctx: QuoteContext) : Expr[String] = {
    import qctx.tasty._
    Expr(x.asTypeTree.tpe.dealias.show)
  }
}
