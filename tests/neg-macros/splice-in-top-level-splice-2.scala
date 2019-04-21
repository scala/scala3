import scala.quoted._

object Foo {
  inline def foo(): Int = ${$x} // error
  def x: Expr[Expr[Int]] = '{ '{1} }
}
