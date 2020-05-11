import scala.quoted._, scala.deriving._

inline def mcr: Any = ${mcrImpl}
def mcrImpl(using s: Scope): s.Expr[Any] = {
  val tpl: (s.Expr[1], s.Expr[2], s.Expr[3]) = ('{1}, '{2}, '{3}).asInstanceOf // FIXME remove the .asInstanceOf
  val tupleExpr = Expr.ofTuple(tpl)
  '{val res: (1, 2, 3) = $tupleExpr; res}
}
