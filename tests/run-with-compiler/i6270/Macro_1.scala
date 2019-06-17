import scala.quoted._
import scala.tasty._

object api {
  inline def (x: => String) reflect : String =
    ${ reflImpl('x) }

  private def reflImpl(x: Expr[String])(implicit refl: Reflection): Expr[String] = {
    import refl._
    x.show.toExpr
  }

  inline def (x: => String) reflectColor : String =
    ${ reflImplColor('x) }

  private def reflImplColor(x: Expr[String])(implicit refl: Reflection): Expr[String] = {
    import refl._
    x.showFormatted.toExpr
  }
}
