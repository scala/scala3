import scala.quoted._

object Macros {
  inline def foo3(): Int = {
    {
      ${ impl() }
    }
  }

  def impl(using s: Scope)(): s.Expr[Int] = '{1}
}