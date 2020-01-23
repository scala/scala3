import scala.quoted._

object Test {
  inline def foo(inline x: Int): Int = ${fooImpl(x, 'x, '{ 'x }, '{ '{ 'x } })}
  def fooImpl(a: Int, b: Expr[Int], c: Expr[QuoteContext ?=> Expr[Int]], d: Expr[QuoteContext ?=> Expr[QuoteContext ?=> Expr[Int]]]): Expr[Int] = ???
}
