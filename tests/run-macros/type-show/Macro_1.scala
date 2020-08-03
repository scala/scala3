import scala.quoted._

object TypeToolbox {
  inline def show[A]: String = ${ showImpl('[A]) }
  private def showImpl[A, B](a: Staged[A])(using qctx: QuoteContext) : Expr[String] = {
    import qctx.tasty._
    Expr(a.show)
  }
}
