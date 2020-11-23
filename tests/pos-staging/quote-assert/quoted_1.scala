import scala.quoted._

object Macros {
  def assertImpl(expr: Expr[Boolean])(using Quotes) =
    '{ if !($expr) then throw new AssertionError(s"failed assertion: ${$expr}") }
}
