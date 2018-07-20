import scala.quoted._

import dotty.tools.dotc.quoted.Toolbox._

object Macros {
  transparent def foo(i: => Int): Int = ~fooImpl('(i))
  def fooImpl(i: Expr[Int]): Expr[Int] = {
    val y: Int = i.run
    y.toExpr
  }
}
