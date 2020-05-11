import scala.quoted._

object Macros {
  inline def foo(): Int = { ${ impl() } }

  def impl()(using s: Scope): s.Expr[Int] = '{1}
}