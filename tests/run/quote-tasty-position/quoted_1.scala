import scala.quoted._

import dotty.tools.dotc.quoted.Toolbox._

import scala.tasty.Context

object Lines {
  inline def line(dummy: Int): Int = ~lineImpl('(dummy))
  def lineImpl(dummy: Expr[Int]): Expr[Int] = {
    val (tree, ctx0) = dummy.toTasty
    implicit val ctx: Context = ctx0
    (tree.pos.startLine + 1).toExpr
  }
}
