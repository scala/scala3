package scala.runtime.quoted

import scala.quoted._
import scala.tasty._

object Matcher {

  type Hole[T /* <: AnyKind */] = T

  def hole[T]: T = ???
  def literal[T]: T = ???

  def unapplySeq(scrutineeExpr: Expr[_])(implicit patternExpr: Expr[_], reflection: Reflection): Option[Seq[Any]] =
    throw new Exception("running on non bootstrapped library")

}
