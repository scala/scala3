import scala.quoted.*

inline def mcr(e: => Any): Any = ${mcrImpl('e)}
def mcrImpl(e: Expr[Any])(using ctx: Quotes): Expr[Any] =
  e match
    case '{ $body } => body
