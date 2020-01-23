import scala.quoted._
import scala.quoted.autolift.{given _}

object Foo {
  inline def foo(): Int = ${bar(${x})} // error
  def x with QuoteContext : Expr[Int] = '{1}
  def bar(i: Int) with QuoteContext : Expr[Int] = i
}
