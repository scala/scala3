import scala.quoted._

import scala.tasty._

object Macros {

  inline def fun(x: Any): Unit = ${ impl('x) }

  def impl(x: Expr[Any])(implicit reflect: Reflection): Expr[Unit] = {
    import reflect._
    error("here is the the argument is " + x.unseal.underlyingArgument.show, x.unseal.underlyingArgument.pos)
    '{}
  }

}
