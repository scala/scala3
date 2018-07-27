import scala.quoted._

object Foo {
  transparent def foo(): Int = ~(~x) // error
  def x: Expr[Expr[Int]] = '( '(1) )
}
