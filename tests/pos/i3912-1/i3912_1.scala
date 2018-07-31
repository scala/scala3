import scala.quoted._

object Macros {
  transparent def foo(): Int = { ~impl() }

  def impl(): Expr[Int] = '(1)
}