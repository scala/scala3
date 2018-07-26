import scala.quoted._

object Macros {
  transparent def foo3(): Int = {
    {
      ~impl()
    }
  }

  def impl(): Expr[Int] = '(1)
}