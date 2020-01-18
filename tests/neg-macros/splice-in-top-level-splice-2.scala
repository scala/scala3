import scala.quoted._

object Foo {
  inline def foo(): Int = ${$x} // error
  def x with QuoteContext : Expr[Expr[Int]] = '{ '{1} }
}
