import scala.quoted.*
inline def foo[T](expr: T => Any): Unit = ${impl('expr)}
def impl(expr: Expr[Any])(using Quotes): Expr[Unit] = '{}
