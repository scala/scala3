import scala.quoted._

inline def mcr(expr: => Any): Any = ${mcrImpl('expr)}

def mcrImpl(expr: Expr[Any])(using Quotes): Expr[Any] =
  import quotes.reflect._
  expr.asTerm.tpe.widen.show
  expr
