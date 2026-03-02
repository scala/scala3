import scala.quoted.*

inline def expandMacro(inline from: Tuple): Any = ${ expandMacroImpl }

def expandMacroImpl(using Quotes): Expr[?] =
  '{ 1 *: EmptyTuple } match
    case '{ ${hd *: tl} : *:[Int, EmptyTuple] } => '{ ??? } // error
