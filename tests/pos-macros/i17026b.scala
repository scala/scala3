import scala.quoted.*

def macroImpl(using Quotes) =
  '{
    def weird[A: ToExpr: Type](a: A)(using quotes: Quotes) =
      '{ Some(${ Expr(a) }) }
  }
