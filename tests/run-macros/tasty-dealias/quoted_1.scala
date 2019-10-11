import scala.quoted._

object Macros {

  inline def dealias[T]: String = ${ impl('[T]) }

  def impl[T](x: TypeTag[T])(given qctx: QuoteContext): Expr[String] = {
    import qctx.tasty.{_, given}
    Expr(x.unseal.tpe.dealias.show)
  }
}
