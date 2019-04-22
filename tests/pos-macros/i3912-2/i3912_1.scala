import scala.quoted._

object Macros {
  inline def foo2(): Unit = ${ impl() }

  def impl(): Expr[Int] = '{1}
}