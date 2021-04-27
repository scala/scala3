import scala.quoted._

inline def mcr(inline x: Any): Any = ${ mcrImpl('x) }
def mcrImpl(expr: Expr[Any])(using Quotes): Expr[Any] =
  import quotes.reflect._
  expr.asTerm match
    case Inlined(_, _, id1) =>
      println(id1.tpe.widen.show)
  '{()}
