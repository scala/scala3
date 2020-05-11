import scala.quoted._

object Macros {
  inline def foo(inline i: Int, dummy: Int, j: Int): Int = ${ bar('i, 'j) }
  def bar(using s: Scope)(x: s.Expr[Int], y: s.Expr[Int]): s.Expr[Int] = '{ ${Expr(x.unliftOrError)} + $y }
}
