import scala.quoted._
def foo[T](using s: Scope)(using s.Type[T]): s.Expr[Any] =
  '{ (using s2: Scope) =>
    type TT = T
    val t = '[TT]
    ???
  }

