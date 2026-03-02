import scala.quoted.*

inline def expandMacro(inline from: Tuple): Any =
  ${ expandMacroImpl }

def expandMacroImpl(using Quotes): Expr[?] =
  '{ 1 *: EmptyTuple } match
    case '{ ($hd: Int) *: ($tl: Tuple) } => '{ ??? }
    case x => throw new MatchError(x.show)
