import scala.quoted._

object TypeToolbox {
  inline def show[A]: String = ${ showImpl('[A]) }
  private def showImpl[A, B](using s: Scope)(a: s.Type[A]): s.Expr[String] = Expr(a.show)
}
