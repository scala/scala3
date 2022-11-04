import scala.quoted.*

inline def mac[T](inline expr: T): T =
  ${ impl('expr) }

class MyMap() extends ExprMap {
  override def transform[T](e: Expr[T])(using Type[T])(using q: Quotes): Expr[T] =
    transformChildren(e)
}

def impl[T: Type](expr: Expr[T])(using quotes: Quotes): Expr[T] = {
  MyMap().transform(expr)
}
