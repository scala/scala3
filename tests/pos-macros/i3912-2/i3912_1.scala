import scala.quoted._

object Macros {
  inline def foo2(): Unit = ${ impl() }

  def impl() given QuoteContext: Expr[Int] = '{1}
}