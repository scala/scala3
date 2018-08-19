import scala.quoted._

object Foo {
  rewrite def foo(): Int = ~(~x) // error
  def x: Expr[Expr[Int]] = '( '(1) )
}
