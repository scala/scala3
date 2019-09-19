import scala.quoted.{_, given}

object Foo {
  inline def foo(x: Int): Int = ${ bar('{ 'x; x }) } // error
  def bar(i: Expr[Int]): Expr[Int] = i
}
