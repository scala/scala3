import scala.quoted._

object Macros {
  rewrite def foo3(): Int = {
    {
      ~impl()
    }
  }

  def impl(): Expr[Int] = '(1)
}