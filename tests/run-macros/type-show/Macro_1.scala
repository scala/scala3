import scala.quoted._

object TypeToolbox {
  inline def show[A]: String = ${ showImpl('[A]) }
  private def showImpl[A, B](a: Type[A]) with (qctx: QuoteContext) : Expr[String] = {
    import qctx.tasty.{_, given _}
    Expr(a.show)
  }
}
