import scala.quoted.*

object Foo {
  inline def foo(): Int = ${$x} // error
  def x(using Quotes): Expr[Expr[Int]] = '{ '{1} }
}
