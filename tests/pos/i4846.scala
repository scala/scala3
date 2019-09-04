import scala.quoted._

object Test {
  inline def foo(inline x: Int): Int = ${fooImpl(x, 'x, '{ 'x }, '{ '{ 'x } })}
  def fooImpl(a: Int, b: Expr[Int], c: Expr[given QuoteContext => Expr[Int]], d: Expr[given QuoteContext => Expr[given QuoteContext => Expr[Int]]]): Expr[Int] = ???
}
