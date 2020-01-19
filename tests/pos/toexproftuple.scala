import scala.quoted._, scala.deriving._
import scala.quoted.{given _}

inline def mcr: Any = ${mcrImpl}
def mcrImpl with (ctx: QuoteContext) : Expr[Any] = {
  val tpl: (Expr[1], Expr[2], Expr[3]) = ('{1}, '{2}, '{3})
  '{val res: (1, 2, 3) = ${Expr.ofTuple(tpl)}; res}
}
