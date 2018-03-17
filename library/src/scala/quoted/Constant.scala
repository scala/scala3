package scala.quoted

import scala.runtime.quoted.Toolbox

object Constant {
  def unapply[T](expr: Expr[T])(implicit runner: Toolbox[T]): Option[T] = runner.toConstantOpt(expr)
}
