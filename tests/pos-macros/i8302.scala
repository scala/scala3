import scala.quoted._
def foo[T](using qctx: QuoteContext, tpe: Staged[T]): Expr[Any] =
  '{ (using qctx: QuoteContext) =>
    type TT = T
    val t = '[TT]
    ???
  }

