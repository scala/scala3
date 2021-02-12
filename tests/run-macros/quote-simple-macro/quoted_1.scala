import scala.quoted.*

object Macros {
  inline def foo(inline i: Int, dummy: Int, j: Int): Int = ${ bar('i, 'j) }
  def bar(x: Expr[Int], y: Expr[Int]) (using Quotes): Expr[Int] = '{ ${Expr(x.valueOrError)} + $y }
}
