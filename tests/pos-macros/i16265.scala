import scala.quoted.*

class Foo(val value: Int)

def foo(exprs: Expr[Any])(using Quotes): Any =
  exprs match
    case '{ $tuple: (Foo *: tail) } =>
      val x = '{ ${tuple}.head.value }
      ???
