import scala.quoted._
import scala.tasty._

object api {
  inline def (x: => String) reflect : String =
    ${ reflImpl('x) }

  private def reflImpl(x: Expr[String])(implicit refl: Reflection): Expr[String] = {
    import refl._
    x.show(the[Context].withoutColors).toExpr
  }

  inline def (x: => String) reflectColor : String =
    ${ reflImplColor('x) }

  private def reflImplColor(x: Expr[String])(implicit refl: Reflection): Expr[String] = {
    import refl._
    x.show(the[Context].withColors).toExpr
  }
}
