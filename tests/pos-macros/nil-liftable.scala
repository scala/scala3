import scala.quoted._

class Test:
  given Liftable[Nil.type] as nilIsLiftable = new Liftable[Nil.type] {
    def toExpr(xs: Nil.type): Quotes ?=> Expr[Nil.type] =
      '{ Nil }
  }
