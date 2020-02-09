import scala.quoted._
import scala.quoted.autolift.{given _}

object Foo {
  inline def foo(): Int = ${bar(${x})} // error
  def x(using QuoteContext): Expr[Int] = '{1}
  def bar(i: Int)(using QuoteContext): Expr[Int] = i
}
