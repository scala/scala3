import scala.quoted._

object Test {
  inline def foo(inline x: Int): Int = ${
    fooImpl(
      x, // error
      'x,
      '{ 'x }, // error
      '{ '{ 'x } } // error
    )
  }
  def fooImpl(a: Int, b: Expr[Int], c: Expr[QuoteContext ?=> Expr[Int]], d: Expr[QuoteContext ?=> Expr[QuoteContext ?=> Expr[Int]]]): Expr[Int] = ???
}
