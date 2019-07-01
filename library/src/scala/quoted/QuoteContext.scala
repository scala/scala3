package scala.quoted

import scala.quoted.show.SyntaxHighlight

/** Quotation context provided by a macro expansion or in the scope of `scala.quoted.run`.
 *  Used to perform all operations on quoted `Expr` or `Type`.
 *
 *  It contains the low-level Typed AST API `tasty` meta-programming API.
 *  This API does not have the static type guarantiees that `Expr` and `Type` provide.
 *
 *  @param tasty Typed AST API. Usage: `def f(qctx: QuoteContext) = { import qctx.tasty._; ... }`.
 */
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
