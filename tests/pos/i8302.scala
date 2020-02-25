import scala.quoted._
def foo[T](using qctx: QuoteContext, tpe: Type[T]): Expr[Any] =
  '{
    type TT = T
    val t = '[TT]
    ???
  }

