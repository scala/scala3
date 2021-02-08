import scala.quoted.*

inline def inner(exprs: Any): Any = ${innerImpl('exprs)}
def innerImpl(exprs: Expr[Any])(using Quotes): Expr[Any] =
  '{$exprs ; ()}

inline def outer(expr: => Any): Any = ${outerImpl('expr)}
def outerImpl(body: Expr[Any])(using Quotes): Expr[Any] = {
  import quotes.reflect.*
  body.asTerm.underlyingArgument.asExpr
}
