import scala.quoted._
import scala.tasty._

object api {
  inline def (x: => T) reflect[T] : String =
    ${ reflImpl('x) }

  private def reflImpl[T](x: Expr[T])(implicit refl: Reflection): Expr[String] = {
    import refl._
    x.unseal.pos.sourceCode.toExpr
  }
}
