import scala.quoted._

object Macros {
  def impl(x: Expr[Int]) with QuoteContext : Expr[Int] = '{ $x + 1 }
}
