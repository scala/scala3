import scala.quoted.{_, given}

object Macros {
  def impl(x: Expr[Int])(given QuoteContext): Expr[Int] = '{ $x + 1 }
}
