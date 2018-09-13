import scala.quoted._
object Macros {
  inline def foo(transparent i: Int): Int = ~bar('(i))

  inline def foo2(transparent i: Int): Int = ~bar('(i + 1))

  def bar(x: Expr[Int]): Expr[Int] = x
}
