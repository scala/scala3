import scala.quoted._

object TypeToolbox {
  inline def show[A]: String = ${ showImpl(Type[A]) }
  private def showImpl[A, B](a: Type[A])(using qctx: QuoteContext) : Expr[String] = {
    import qctx.tasty._
    Expr(a.show)
  }
}
