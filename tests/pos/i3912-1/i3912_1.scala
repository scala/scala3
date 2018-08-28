import scala.quoted._

object Macros {
  rewrite def foo(): Int = { ~impl() }

  def impl(): Expr[Int] = '(1)
}