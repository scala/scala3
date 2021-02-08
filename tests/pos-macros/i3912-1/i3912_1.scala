import scala.quoted.*

object Macros {
  inline def foo(): Int = { ${ impl() } }

  def impl()(using Quotes): Expr[Int] = '{1}
}