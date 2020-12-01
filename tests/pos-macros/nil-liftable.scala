import scala.quoted._

class Test:
  given NilIsLiftable: Liftable[Nil.type] = new Liftable[Nil.type] {
    def toExpr(xs: Nil.type): Quotes ?=> Expr[Nil.type] =
      '{ Nil }
  }
