import scala.quoted._

inline def inner(exprs: Any): Any = ${innerImpl('exprs)}
def innerImpl(exprs: Expr[Any])(using QuoteContext): Expr[Any] =
  '{$exprs ; ()}

inline def outer(expr: => Any): Any = ${outerImpl('expr)}
def outerImpl(body: Expr[Any])(using QuoteContext): Expr[Any] = {
  import qctx.reflect._
  body.unseal.underlyingArgument.seal
}
