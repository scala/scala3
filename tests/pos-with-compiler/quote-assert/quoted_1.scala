import scala.quoted._

object Macros {
  def assertImpl(expr: Expr[Boolean]) given QuoteContext =
    '{ if !($expr) then throw new AssertionError(s"failed assertion: ${$expr}") }
}
