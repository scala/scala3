import scala.quoted.*

inline def f[T](inline code: =>T): Any =
  ${ create[T]('{ () => code }) }

def create[T: Type](code: Expr[() => T])(using Quotes): Expr[Any] =
  '{ identity($code) }
