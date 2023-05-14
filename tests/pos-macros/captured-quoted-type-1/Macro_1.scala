import scala.quoted.*

inline def myMacro: Any = ${ myMacroExpr }

def myMacroExpr(using Quotes): Expr[Any] =
  '{
    def f[Z] =
      ${ identity('{ val y: Z = ??? }) }
    42
  }
