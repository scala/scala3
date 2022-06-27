import scala.quoted.*

class Wrapper[T](t: T):
  inline def showType: String = ${ Wrapper.showTypeImpl[T]}

object Wrapper:
  def showTypeImpl[U](using Quotes): Expr[String] = Expr("foo")
