import scala.quoted.*

inline def mcr: Any = ${mcrImpl}
def mcrImpl(using ctx: Quotes): Expr[Any] = {
  val tpl: Expr[1] = '{1}
  '{()}
}
