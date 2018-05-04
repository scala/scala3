import scala.quoted._
object Macros {
  inline def foo(inline i: Int): Int = ~bar('(i))
  def bar(x: Expr[Int]): Expr[Int] = x

  inline def foo2(inline i: Int): Int = ~bar('(i + 1))
  def bar2(x: Expr[Int]): Expr[Int] = x
}
