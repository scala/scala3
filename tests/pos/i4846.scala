import scala.quoted._

object Test {
  inline def foo(inline x: Int): Int = ${fooImpl(x, 'x, '{ 'x }, '{ '{ 'x } })}
  def fooImpl(a: Int, b: Expr[Int], c: Expr[Expr[Int]], d: Expr[Expr[Expr[Int]]]): Expr[Int] = ???
}
