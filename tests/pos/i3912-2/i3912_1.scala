import scala.quoted._

object Macros {
  rewrite def foo2(): Unit = ~impl()

  def impl(): Expr[Int] = '(1)
}