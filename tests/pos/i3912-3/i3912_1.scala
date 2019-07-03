import scala.quoted._

object Macros {
  inline def foo3(): Int = {
    {
      ${ impl() }
    }
  }

  def impl() given QuoteContext: Expr[Int] = '{1}
}