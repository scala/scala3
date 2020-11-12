import scala.quoted._
def foo[T](using qctx: QuoteContext, tpe: Type[T]): Expr[Any] =
  '{ (using qctx: QuoteContext) =>
    type TT = T
    val t = Type.of[TT]
    ???
  }

