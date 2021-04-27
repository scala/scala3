import scala.quoted._

object Macros {
  def impl(x: Expr[Int])(using Quotes): Expr[Int] = '{ $x + 1 }
}
