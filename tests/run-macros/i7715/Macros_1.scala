import scala.quoted.{ given _, _ }

inline def mcr(e: => Any): Any = ${mcrImpl('e)}
def mcrImpl(e: Expr[Any]) with (ctx: QuoteContext) : Expr[Any] =
  e match
    case '{ $body } => body
