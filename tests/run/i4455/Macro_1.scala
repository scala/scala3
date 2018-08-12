import scala.quoted._
object Macros {
  rewrite def foo(transparent i: Int): Int = ~bar('(i))

  rewrite def foo2(transparent i: Int): Int = ~bar('(i + 1))

  def bar(x: Expr[Int]): Expr[Int] = x
}
