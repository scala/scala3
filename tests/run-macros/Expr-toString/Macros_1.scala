import scala.quoted.*

inline def showToString(inline x: Any): String = ${ macroImpl('x) }

private def macroImpl(x: Expr[Any])(using Quotes): Expr[String] =
  Expr(x.toString)
