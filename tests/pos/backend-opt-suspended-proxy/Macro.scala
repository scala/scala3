import scala.quoted.*
object M:
  inline def id[T](inline x: T): T = ${ idImpl('x) }
  def idImpl[T: Type](x: Expr[T])(using Quotes): Expr[T] = x
