import scala.quoted.*

object Macro {
  object MyMatcher {
    def unapply(expr: Expr[Any])(using Quotes): Option[Expr[Int]] = expr match {
      case '{ (${a}: Int) + (${_}: Int) } => Some(a)
      case _ => None
    }
  }

  def foo(x: Int): Int = x - 1

  def impl(expr: Expr[Any])(using Quotes): Expr[(Int, Int)] = expr match
    case '{foo(${bound@MyMatcher(x)})}=> '{($bound, $x)}

  inline def macr(inline x: Int): (Int, Int) = ${impl('x)}
}
