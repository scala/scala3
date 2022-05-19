import scala.quoted.*

inline def myMacro: Any = ${ myMacroExpr('{1}) }

def myMacroExpr(x: Expr[Int])(using Quotes): Expr[Any] =
  '{
    def f(using q1: Quotes) = '{ 1 + ${Expr($x)} }
    ()
  }
