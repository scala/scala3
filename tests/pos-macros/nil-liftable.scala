import scala.quoted.*

class Test:
  given NilToExpr: ToExpr[Nil.type] {
    def apply(xs: Nil.type)(using Quotes): Expr[Nil.type] =
      '{ Nil }
  }
