package example

import scala.quoted.*

object A {
  inline def f(inline a: Any): Boolean = ${ impl('a) }

  def impl(a: Expr[Any])(using Quotes): Expr[Boolean] = {
    a match {
      case '{ new String($x: Array[Byte]) } => Expr(true)
      case _ => quotes.reflect.report.errorAndAbort("Expected match", a)
    }
  }
}
