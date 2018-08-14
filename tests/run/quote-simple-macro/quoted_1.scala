import scala.quoted._

object Macros {
  transparent def foo(i: Int & Constant, dummy: Int, j: Int): Int = ~bar(i, '(j))
  def bar(x: Int, y: Expr[Int]): Expr[Int] = '{ ~x.toExpr + ~y }
}
