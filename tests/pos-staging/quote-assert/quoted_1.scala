import scala.quoted.{_, given}

object Macros {
  def assertImpl(expr: Expr[Boolean])(given QuoteContext) =
    '{ if !($expr) then throw new AssertionError(s"failed assertion: ${$expr}") }
}
