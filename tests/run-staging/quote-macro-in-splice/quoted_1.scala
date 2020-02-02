import scala.quoted._

object Macros {
  def impl(x: Expr[Int])(using QuoteContext): Expr[Int] = '{ $x + 1 }
}
