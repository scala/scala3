import scala.quoted._

object Test {
  inline def foo(inline x: Int): Int = ${fooImpl(x)}
  def fooImpl(a: Int): Expr[Int] = ???
}
