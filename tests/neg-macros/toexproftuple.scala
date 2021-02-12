import scala.quoted._, scala.deriving.*

inline def mcr: Any = ${mcrImpl}
def mcrImpl(using ctx: Quotes): Expr[Any] = {
  val tpl: (Expr[1], Expr[2], Expr[3]) = ('{1}, '{2}, '{3})
  '{val res: (1, 3, 3) = ${Expr.ofTuple(tpl)}; res}  // error

  val tpl2: (Expr[1], 2, Expr[3]) = ('{1}, 2, '{3})
  '{val res = ${Expr.ofTuple(tpl2)}; res}  // error
}
