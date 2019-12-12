import scala.quoted.{ given, _ }

inline def mcr(e: => Any): Any = ${mcrImpl('e)}
def mcrImpl(e: Expr[Any])(given ctx: QuoteContext): Expr[Any] =
  e match
    case '{ $body } => body
