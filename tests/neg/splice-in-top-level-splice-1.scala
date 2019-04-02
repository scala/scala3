import scala.quoted._
import scala.quoted.autolift._

object Foo {
  inline def foo(): Int = ${bar(${x})} // error
  def x: Expr[Int] = '{1}
  def bar(i: Int): Expr[Int] = i
}
