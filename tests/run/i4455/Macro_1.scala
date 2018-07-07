import scala.quoted._
object Macros {
  inline def foo(inline i: Int): Int = ~bar(i.toExpr)

  inline def foo2(inline i: Int): Int = ~bar((i + 1).toExpr)

  def bar(x: Expr[Int]): Expr[Int] = x
}
