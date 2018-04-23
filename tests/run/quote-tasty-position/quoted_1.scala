import scala.quoted._

import dotty.tools.dotc.quoted.Toolbox._

object Lines {
  inline def line(dummy: Int): Int = ~lineImpl('(dummy))
  def lineImpl(dummy: Expr[Int]): Expr[Int] = (dummy.toTasty.pos.startLine + 1).toExpr
}
