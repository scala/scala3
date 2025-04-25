import scala.quoted._

inline def passThorugh(inline condition: Boolean): Any =
  ${ passThorughImpl('{condition}) }

def passThorughImpl(condition: Expr[Boolean])(using Quotes): Expr[Any] = condition
