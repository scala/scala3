import scala.quoted._

object Macro {

  inline def foo(b: Boolean): Int = { // error
    if (b) ${ bar(true) }
    else ${ bar(false) }
  }

  def bar(using s: Scope)(b: Boolean): s.Expr[Int] = if (b) '{1} else '{0}
}
