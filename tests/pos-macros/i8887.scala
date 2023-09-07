import scala.quoted._
inline def foo(x: Any): Any = ${ expr[x.type] }
def expr[X](using Quotes): Expr[Any] = ???
