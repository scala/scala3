import scala.quoted._

object Foo {
  inline def foo(): Int = ${$x} // error
  def x: Staged[Expr[Int]] = '{ '{1} }
}
