import scala.quoted._

object Macros {
  inline def foo3(): Int = {
    {
      ${ impl() }
    }
  }

  def impl()(using Quotes): Expr[Int] = '{1}
}