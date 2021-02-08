import scala.quoted.*

object Macros {
  inline def foo3(): Int = {
    {
      ${ impl() }
    }
  }

  def impl()(using Quotes): Expr[Int] = '{1}
}