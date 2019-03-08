import scala.quoted._

object Macros {
  inline def foo3(): Int = {
    {
      ${ impl() }
    }
  }

  def impl(): Expr[Int] = '{1}
}