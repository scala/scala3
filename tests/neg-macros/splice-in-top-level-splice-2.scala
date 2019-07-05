import scala.quoted._

object Foo {
  inline def foo(): Int = ${$x} // error
  def x given QuoteContext: Expr[Expr[Int]] = '{ '{1} }
}
