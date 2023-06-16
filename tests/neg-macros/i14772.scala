import scala.quoted.*

object A {
  transparent inline def foo(a: Any): Any = ${ impl('a) }

  def impl(a: Expr[Any])(using Quotes)/*: Expr[Any]*/ = {
    foo(a)    // error
    ???
  }
}