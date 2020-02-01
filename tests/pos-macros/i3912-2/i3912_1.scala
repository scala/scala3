import scala.quoted._

object Macros {
  inline def foo2(): Unit = ${ impl() }

  def impl()(using QuoteContext): Expr[Int] = '{1}
}