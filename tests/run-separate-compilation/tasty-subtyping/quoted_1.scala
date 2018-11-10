import scala.quoted._

import scala.tasty._

object Macros {

  inline def isTypeEqual[T, U]: Boolean =
    ~isTypeEqualImpl('[T], '[U])

  inline def isSubTypeOf[T, U]: Boolean =
    ~isSubTypeOfImpl('[T], '[U])

  def isTypeEqualImpl[T, U](t: Type[T], u: Type[U])(implicit reflect: Reflection): Expr[Boolean] = {
    import reflect._
    val isTypeEqual = t.reflect.tpe =:= u.reflect.tpe
    isTypeEqual.toExpr
  }

  def isSubTypeOfImpl[T, U](t: Type[T], u: Type[U])(implicit reflect: Reflection): Expr[Boolean] = {
    import reflect._
    val isTypeEqual = t.reflect.tpe <:< u.reflect.tpe
    isTypeEqual.toExpr
  }
}
