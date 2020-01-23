import scala.quoted._

object Macros {

  inline def dealias[T]: String = ${ impl('[T]) }

  def impl[T](x: quoted.Type[T]) with (qctx: QuoteContext) : Expr[String] = {
    import qctx.tasty.{_, given _}
    Expr(x.unseal.tpe.dealias.show)
  }
}
