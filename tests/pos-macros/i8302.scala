import scala.quoted.*
def foo[T](using Quotes, Type[T]): Expr[Any] =
  '{ (q: Quotes) ?=>
    type TT = T
    val t = Type.of[TT]
    ???
  }
