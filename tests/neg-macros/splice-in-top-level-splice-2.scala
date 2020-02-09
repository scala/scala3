import scala.quoted._

object Foo {
  inline def foo(): Int = ${$x} // error
  def x(using QuoteContext): Expr[Expr[Int]] = '{ '{1} }
}
