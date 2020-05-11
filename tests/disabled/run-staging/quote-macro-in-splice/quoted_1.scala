import scala.quoted._

object Macros {
  def impl(using s: Scope)(x: s.Expr[Int]): s.Expr[Int] = '{ $x + 1 }
}
