package scala.runtime.quoted

import scala.annotation.implicitNotFound
import scala.quoted.Expr

@implicitNotFound("Could not find implicit quoted.Toolbox. Default toolbox can be imported with `import dotty.tools.dotc.quoted.Toolbox._`")
trait Toolbox[T] {
  def run(expr: Expr[T]): T
  def show(expr: Expr[T]): String
  // TODO add def toTasty
  def toConstantOpt(expr: Expr[T]): Option[T] // TODO remove and implement with toTasty
}
