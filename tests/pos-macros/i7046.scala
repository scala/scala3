import scala.quoted._

inline def mcr: Any = ${mcrImpl}
def mcrImpl(using ctx: QuoteContext): Expr[Any] = {
  val tpl: Expr[1] = '{1}
  '{()}
}
