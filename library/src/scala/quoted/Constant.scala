package scala.quoted

import scala.runtime.quoted.Runner

object Constant {
  def unapply[T](expr: Expr[T])(implicit runner: Runner[T]): Option[T] = runner.toConstantOpt(expr)
}
