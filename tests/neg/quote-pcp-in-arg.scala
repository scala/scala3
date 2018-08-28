import scala.quoted._

object Foo {
  rewrite def foo(x: Int): Int = ~bar('{ '(x); x }) // error
  def bar(i: Expr[Int]): Expr[Int] = i
}
