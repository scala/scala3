import scala.quoted._

object Macros {
  def impl(x: Expr[Int]): Expr[Int] = '{ $x + 1 }
}
