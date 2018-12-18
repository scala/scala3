import scala.quoted._

import scala.tasty._

object Macros {

  inline def show(x: => Any): String = ~impl('(x))

  def impl(x: Expr[Any])(implicit reflect: Reflection): Expr[String] = {
    import reflect._
    x.unseal.show.toExpr
  }

}
