import scala.quoted.*

inline def id[T](v: => T): T = ${ idImpl('v) }
def idImpl[T: Type](v: Expr[T])(using Quotes): Expr[T] = v

inline def trace(inline expr: Any): Any = ${ traceImpl('expr) }
def traceImpl(expr: Expr[Any])(using Quotes): Expr[Any] = expr
