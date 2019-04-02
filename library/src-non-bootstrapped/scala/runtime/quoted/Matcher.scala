package scala.runtime.quoted

import scala.quoted._
import scala.tasty._

object Matcher {

  def unapply[Tup <: Tuple](scrutineeExpr: Expr[_])(implicit patternExpr: Expr[_], reflection: Reflection): Option[Tup] =
    throw new Exception("running on non bootstrapped library")

}
