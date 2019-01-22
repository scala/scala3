package scala.internal.quoted

import scala.quoted.Expr
import scala.tasty.Reflection

object Matcher {

  def unapply[Tup <: Tuple](scrutineeExpr: Expr[_])(implicit patternExpr: Expr[_], reflection: Reflection): Option[Tup] =
    throw new Exception("running on non bootstrapped library")

}
