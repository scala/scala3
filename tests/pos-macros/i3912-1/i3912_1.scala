import scala.quoted._

object Macros {
  inline def foo(): Int = { ${ impl() } }

  def impl() given QuoteContext: Expr[Int] = '{1}
}