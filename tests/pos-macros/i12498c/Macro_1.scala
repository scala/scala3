import scala.quoted.*


class Wrapper[T](t: T):
  inline def emptyList: List[T] = ${ Wrapper.emptyListImpl : Expr[List[T]] }

object Wrapper:
  def emptyListImpl(using Quotes): Expr[List[Nothing]] = Expr(Nil)
