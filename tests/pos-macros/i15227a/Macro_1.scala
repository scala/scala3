import scala.quoted.*

inline def mac[T](inline expr: T): T =
  ${ impl('expr) }

def impl[T: Type](expr: Expr[T])(using Quotes): Expr[T] = {
  import quotes.reflect.*
  val expr2 = expr.asTerm.asExpr

  assert(expr == expr2)
  assert(expr.hashCode() == expr2.hashCode())

  expr

}
