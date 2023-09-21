import scala.quoted.*

inline def foo[U](u: U): U = ${ fooImpl[U]('u) }

def fooImpl[U: Type](u: Expr[U])(using Quotes): Expr[U] = '{
  def f[T](x: T): T = ${ identity('{ x: T }) }
  f[U]($u)
}
