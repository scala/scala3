import scala.quoted.*


class Wrapper[T](t: T):
  inline def nothing: T = ${ Wrapper.nothing : Expr[T] }

object Wrapper:
  def nothing(using Quotes): Expr[Nothing] = '{???}
