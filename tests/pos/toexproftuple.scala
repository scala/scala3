import scala.quoted.{_, given}, scala.deriving._
import scala.quoted.given

inline def mcr: Any = ${mcrImpl}
def mcrImpl(given ctx: QuoteContext): Expr[Any] = {
  val tpl: (Expr[1], Expr[2], Expr[3]) = ('{1}, '{2}, '{3})
  '{val res: (1, 2, 3) = ${tpl.toExprOfTuple}; res}
}
