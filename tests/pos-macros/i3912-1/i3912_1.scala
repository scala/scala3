import scala.quoted._

object Macros {
  inline def foo(): Int = { ${ impl() } }

  def impl()(using QuoteContext): Expr[Int] = '{1}
}