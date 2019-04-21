import scala.quoted._
import scala.tasty._

object TypeToolbox {
  inline def show[A]: String = ${ showImpl('[A]) }
  private def showImpl[A, B](a: Type[A])(implicit refl: Reflection): Expr[String] = {
    import refl._
    a.show.toExpr
  }
}
