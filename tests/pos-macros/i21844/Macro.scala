import scala.quoted.*

object Macro:
  inline def foo = ${ fooImpl }
  def fooImpl(using Quotes): Expr[Int] =
    '{ 123 }
