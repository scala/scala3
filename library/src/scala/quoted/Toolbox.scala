package scala.quoted

import scala.annotation.implicitNotFound

@implicitNotFound("Could not find implicit quoted.Toolbox.\n\nDefault toolbox can be instantiated with:\n  `implicit val toolbox: scala.quoted.Toolbox = dotty.tools.dotc.quoted.Toolbox.make`\n\nIf only needed once it can also be imported with:\n `import dotty.tools.dotc.quoted.Toolbox._`")
trait Toolbox {
  def run[T](expr: Expr[T]): T
  def show[T](expr: Expr[T]): String
}
