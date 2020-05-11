import scala.quoted._

object Macros {
  inline def foo2(): Unit = ${ impl() }

  def impl()(using s: Scope): s.Expr[Int] = '{1}
}