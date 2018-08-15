import scala.quoted._
object Macros {
  transparent def foo(i: Int & Constant): Int = ~bar('(i))

  transparent def foo2(i: Int & Constant): Int = ~bar('(i + 1))

  def bar(x: Expr[Int]): Expr[Int] = x
}
