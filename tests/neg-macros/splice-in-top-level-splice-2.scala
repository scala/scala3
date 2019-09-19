import scala.quoted.{_, given}

object Foo {
  inline def foo(): Int = ${$x} // error
  def x(given QuoteContext): Expr[Expr[Int]] = '{ '{1} }
}
