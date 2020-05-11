import scala.quoted._

inline def inner(exprs: Any): Any = ${innerImpl('exprs)}
def innerImpl(using s: Scope)(exprs: s.Expr[Any]): s.Expr[Any] =
  '{$exprs ; ()}

inline def outer(expr: => Any): Any = ${outerImpl('expr)}
def outerImpl(using s: Scope)(body: s.Expr[Any]): s.Expr[Any] = {
  import s.tasty._
  body.underlyingArgument.seal
}
