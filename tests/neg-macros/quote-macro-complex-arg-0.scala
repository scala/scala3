import scala.quoted._

object Macros {
  inline def foo(inline i: Int, dummy: Int, j: Int): Int = ${ bar(i + 1, 'j) } // error: i + 1 is not a parameter or field reference
  def bar(using s: Scope)(x: Int, y: s.Expr[Int]): s.Expr[Int] = '{ ${Expr(x)} + $y }
}
