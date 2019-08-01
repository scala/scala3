import scala.quoted._
import given scala.quoted.autolift._

object Foo {
  inline def foo(): Int = ${bar(${x})} // error
  def x given QuoteContext: Expr[Int] = '{1}
  def bar(i: Int) given QuoteContext: Expr[Int] = i
}
