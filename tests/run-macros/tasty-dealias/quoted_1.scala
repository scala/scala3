import scala.quoted.{_, given}

object Macros {

  inline def dealias[T]: String = ${ impl('[T]) }

  def impl[T](x: quoted.Type[T])(given qctx: QuoteContext): Expr[String] = {
    import qctx.tasty._
    x.unseal.tpe.dealias.show.toExpr
  }
}
