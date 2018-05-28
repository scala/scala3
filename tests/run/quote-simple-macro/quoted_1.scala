import scala.quoted._

object Macros {
  inline def foo(inline i: Int, dummy: Int, j: Int): Int = ~bar(i + 1, '(j))
  def bar(x: Int, y: Expr[Int]): Expr[Int] = '{ ~x.toExpr + ~y }
}
