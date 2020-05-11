import scala.quoted._

inline def mcr: Any = ${mcrImpl}
def mcrImpl(using s: Scope): s.Expr[Any] = {
  val tpl: s.Expr[1] = '{1}.asInstanceOf[s.Expr[1]] // FIXME: remove asInstanceOf
  '{()}
}
