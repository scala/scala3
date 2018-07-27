import scala.quoted._

object Foo {
  transparent def foo(): Int = ~bar(~x) // error
  def x: Expr[Int] = '(1)
  def bar(i: Int): Expr[Int] = i.toExpr
}
