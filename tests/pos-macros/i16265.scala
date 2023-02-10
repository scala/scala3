import scala.quoted.*

class Foo(val value: Int)

def foo(exprs: Expr[Any])(using Quotes): Any =
  exprs match
    case '{ type tail <: Tuple; $tuple: (Foo *: tail) } => // FIXME infer bounds of tail
      val x = '{ ${tuple}.head.value }
      ???
