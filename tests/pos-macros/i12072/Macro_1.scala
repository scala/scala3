import scala.quoted.*

object M {

  transparent inline def f(inline s: String): String | Null =
    ${ f('s) }

  def f(s: Expr[String])(using Quotes): Expr[String | Null] = {
    s.valueOrError // required
    '{ null }
  }
}
