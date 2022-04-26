import scala.quoted.*

inline def myMacro: Any = ${ myMacroExpr }

def myMacroExpr[T: Type](using Quotes): Expr[Any] =
  '{
    def f[Z] =
      ${ identity('{ val y: (T, Z) = ??? }) }
    42
  }
