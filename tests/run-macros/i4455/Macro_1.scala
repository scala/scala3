import scala.quoted._
object Macros {
  inline def foo(inline i: Int): Int = ${ bar('i) }

  inline def foo2(inline i: Int): Int = ${ bar('{i + 1}) }

  def bar(using s: Scope)(x: s.Expr[Int]): s.Expr[Int] = x
}
