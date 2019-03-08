import scala.quoted._

object Macros {
  inline def foo(): Int = { ${ impl() } }

  def impl(): Expr[Int] = '{1}
}