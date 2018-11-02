import scala.quoted._

import scala.tasty._

class LineNumber(val value: Int) {
  override def toString: String = value.toString
}

object LineNumber {

  implicit inline def line: LineNumber = ~lineImpl

  def lineImpl(implicit tasty: Tasty): Expr[LineNumber] = {
    import tasty._
    '(new LineNumber(~rootPosition.startLine.toExpr))
  }

}
