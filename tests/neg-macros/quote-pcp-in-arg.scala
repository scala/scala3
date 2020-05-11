import scala.quoted._

object Foo {
  inline def foo(x: Int): Int = ${ bar('{ 'x; x }) } // error
  def bar(using s: Scope)(i: s.Expr[Int]): s.Expr[Int] = i
}
