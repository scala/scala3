import scala.quoted.*

object Foo {
  inline def foo(): Int = ${bar(${x})} // error
  def x(using Quotes): Expr[Int] = '{1}
  def bar(i: Int)(using Quotes): Expr[Int] = Expr(i)
}
