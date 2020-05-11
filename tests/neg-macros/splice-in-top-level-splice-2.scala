import scala.quoted._

object Foo {
  inline def foo(): Int = ${$x} // error
  def x(using s: Scope): s.Expr[s.Expr[Int]] = '{ '{1} } // error
}
