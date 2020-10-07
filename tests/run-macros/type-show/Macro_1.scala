import scala.quoted._

object TypeToolbox {
  inline def show[A]: String = ${ showImpl('[A]) }
  private def showImpl[A, B](a: Type[A])(using qctx: QuoteContext) : Expr[String] = {
    import qctx.reflect._
    Expr(a.show)
  }
}
