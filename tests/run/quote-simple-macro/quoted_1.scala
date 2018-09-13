import scala.quoted._

object Macros {
  inline def foo(transparent i: Int, dummy: Int, j: Int): Int = ~bar(i, '(j))
  def bar(x: Int, y: Expr[Int]): Expr[Int] = '{ ~x.toExpr + ~y }
}
