import scala.quoted.*

class Test:
  given NilToExpr: ToExpr[Nil.type] with {
    def apply(xs: Nil.type)(using Quotes): Expr[Nil.type] =
      '{ Nil }
  }
