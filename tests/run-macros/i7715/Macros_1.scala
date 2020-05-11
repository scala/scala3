import scala.quoted._

inline def mcr(e: => Any): Any = ${mcrImpl('e)}
def mcrImpl(using s: Scope)(e: s.Expr[Any]): s.Expr[Any] =
  e match
    case '{ $body } => body
