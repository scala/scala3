import scala.quoted._
def foo[T](using Quotes, Type[T]): Expr[Any] =
  '{ (using q: Quotes) =>
    type TT = T
    val t = Type.of[TT]
    ???
  }

