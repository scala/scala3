package scala.runtime.quoted

import scala.annotation.implicitNotFound
import scala.quoted.{Expr, Type}
import scala.tasty.trees

@implicitNotFound("Could not find implicit quoted.Toolbox. Default toolbox can be imported with `import dotty.tools.dotc.quoted.Toolbox._`")
trait Toolbox[T] {
  def run(expr: Expr[T]): T
  def show(expr: Expr[T]): String
  def toTasty(expr: Expr[T]): trees.Term
  def toTasty(expr: Type[T]): trees.TypeTree
}
