import scala.quoted._

import scala.tasty._

object Macros {

  rewrite def isTypeEqual[T, U]: Boolean =
    ~isTypeEqualImpl('[T], '[U])(TopLevelSplice.tastyContext) // FIXME infer TopLevelSplice.tastyContext within top level ~

  rewrite def isSubTypeOf[T, U]: Boolean =
    ~isSubTypeOfImpl('[T], '[U])(TopLevelSplice.tastyContext) // FIXME infer TopLevelSplice.tastyContext within top level ~

  def isTypeEqualImpl[T, U](t: Type[T], u: Type[U])(implicit tasty: Tasty): Expr[Boolean] = {
    import tasty._
    val isTypeEqual = t.toTasty.tpe =:= u.toTasty.tpe
    isTypeEqual.toExpr
  }

  def isSubTypeOfImpl[T, U](t: Type[T], u: Type[U])(implicit tasty: Tasty): Expr[Boolean] = {
    import tasty._
    val isTypeEqual = t.toTasty.tpe <:< u.toTasty.tpe
    isTypeEqual.toExpr
  }
}
