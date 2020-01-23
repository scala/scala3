import scala.quoted._

object Macros {
  inline def foo3(): Int = {
    {
      ${ impl() }
    }
  }

  def impl() with QuoteContext : Expr[Int] = '{1}
}