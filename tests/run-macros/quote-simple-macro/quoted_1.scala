import scala.quoted._
import scala.quoted.autolift.{given _}

object Macros {
  inline def foo(inline i: Int, dummy: Int, j: Int): Int = ${ bar('i, 'j) }
  def bar(x: Expr[Int], y: Expr[Int]) with QuoteContext : Expr[Int] = '{ ${x.value} + $y }
}
