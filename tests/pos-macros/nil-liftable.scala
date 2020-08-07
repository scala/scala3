import scala.quoted._

class Test:
  given NilIsLiftable as Liftable[Nil.type] = new Liftable[Nil.type] {
    def toExpr(xs: Nil.type): QuoteContext ?=> Expr[Nil.type] =
      '{ Nil }
  }
