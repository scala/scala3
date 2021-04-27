import scala.quoted.*

object Test {
  inline def foo(inline x: Int): Int = ${
    fooImpl(
      x, // error
      'x,
      '{ 'x }, // error
      '{ '{ 'x } } // error
    )
  }
  def fooImpl(a: Int, b: Expr[Int], c: Expr[Quotes ?=> Expr[Int]], d: Expr[Quotes ?=> Expr[Quotes ?=> Expr[Int]]]): Expr[Int] = ???
}
