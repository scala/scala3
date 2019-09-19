import scala.quoted.{_, given}

object Macros {
  inline def foo2(): Unit = ${ impl() }

  def impl()(given QuoteContext): Expr[Int] = '{1}
}