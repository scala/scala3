import scala.quoted.*

inline def myMacro = ${ myMacroExpr }

def myMacroExpr(using Quotes) =
  import quotes.reflect.*

  '{ def innerMethod = (_: String) ?=> ???; () }.asTerm match
    case block @ Inlined(_, _, Block(List(defdef: DefDef), _)) =>
      val rhs =
        given Quotes = defdef.symbol.asQuotes
        '{ (x: String) ?=> ??? }.asTerm

      Block(List(DefDef(defdef.symbol, _ => Some(rhs))), '{}.asTerm).asExprOf[Unit]
