import scala.quoted._

import scala.tasty._

object Macros {

  inline def fun(x: Any): Unit = ${ impl('x) }

  inline def fun2(x: =>Any): Unit = ${ impl('x) }

  inline def fun3[T]: Unit = ${ impl2('[T]) }

  def impl(x: Expr[Any])(implicit reflect: Reflection): Expr[Unit] = {
    import reflect._
    error("here is the the argument is " + x.unseal.underlyingArgument.showCode, x.unseal.underlyingArgument.pos)
    '{}
  }

  def impl2[T](x: quoted.Type[T])(implicit reflect: Reflection): Expr[Unit] = {
    import reflect._
    error("here is the the argument is " + x.unseal.showCode, x.unseal.pos)
    '{}
  }

}
