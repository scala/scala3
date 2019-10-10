import scala.quoted._

object TypeToolbox {
  inline def show[A]: String = ${ showImpl('[A]) }
  private def showImpl[A, B](a: TypeTag[A])(given qctx: QuoteContext): Expr[String] = {
    import qctx.tasty.{_, given}
    Expr(a.show)
  }
}
