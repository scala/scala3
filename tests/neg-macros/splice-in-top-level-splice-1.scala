import scala.quoted._

object Foo {
  inline def foo(): Int = ${bar(${x})} // error
  def x(using s: Scope): s.Expr[Int] = '{1}
  def bar(using s: Scope)(i: Int): s.Expr[Int] = s.Expr(i)
}
