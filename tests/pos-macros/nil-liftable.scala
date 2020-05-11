import scala.quoted._

class Test:
  given NilIsLiftable(using s: Scope) as s.Liftable[Nil.type] = new s.Liftable[Nil.type] {
    def toExpr(xs: Nil.type): s.Expr[Nil.type] =
      '{ Nil }
  }
