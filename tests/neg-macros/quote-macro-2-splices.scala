import scala.quoted.*

object Macro {

  inline def foo(b: Boolean): Int = { // error
    if (b) ${ bar(true) }
    else ${ bar(false) }
  }

  def bar(b: Boolean)(using Quotes): Expr[Int] = if (b) '{1} else '{0}
}
