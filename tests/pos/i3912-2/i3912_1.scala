import scala.quoted._

object Macros {
  transparent def foo2(): Unit = ~impl()

  def impl(): Expr[Int] = '(1)
}