package scala.quoted

import scala.quoted.show.SyntaxHighlight

class QuoteContext(val tasty: scala.tasty.Reflection) {

  def show[T](expr: Expr[T], syntaxHighlight: SyntaxHighlight): String = {
    import tasty._
    expr.unseal.show(syntaxHighlight)
  }

  def show[T](tpe: Type[T], syntaxHighlight: SyntaxHighlight): String = {
    import tasty._
    tpe.unseal.show(syntaxHighlight)
  }

}

object QuoteContext {
  // TODO remove in 0.18
  // For backward compat with macros
  @deprecated("Provide scala.quoted.QuoteContext instead of using a scala.tasty.Reflection", "0.17")
  implicit def reflectionToQuoteContext(implicit reflect: tasty.Reflection): scala.quoted.QuoteContext =
    new scala.quoted.QuoteContext(reflect)

  def macroContext: QuoteContext = throw new Exception("Not in inline macro.")
}
