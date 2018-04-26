import scala.quoted._

import dotty.tools.dotc.quoted.Toolbox._

class LineNumber(val value: Int) {
  override def toString: String = value.toString
}

object LineNumber {
  implicit inline def line[T]: LineNumber = ~impl2('[T])

  def impl2[T](x: Type[T]): Expr[LineNumber] = {
    val pos = {
      val (tree, ctx) = x.toTasty
      tree.pos(ctx)
    }
    '(new LineNumber(~pos.startLine.toExpr))
  }
}
