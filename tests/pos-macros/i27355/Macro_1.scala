import scala.quoted.*

inline def identity[T](inline x: T): T = ${ identityImpl('x) }

def identityImpl[T: Type](x: Expr[T])(using Quotes): Expr[T] =
  object Id extends ExprMap:
    def transform[U](e: Expr[U])(using Type[U])(using Quotes): Expr[U] = transformChildren(e)
  Id.transform(x)
