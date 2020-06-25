package scala.quoted

import scala.quoted.show.SyntaxHighlight

/** Quotation context provided by a macro expansion or in the scope of `scala.quoted.run`.
 *  Used to perform all operations on quoted `Expr` or `Type`.
 *
 *  It contains the low-level Typed AST API `tasty` metaprogramming API.
 *  This API does not have the static type guarantiees that `Expr` and `Type` provide.
 *
 *  @param tasty Typed AST API. Usage: `def f(qctx: QuoteContext) = { import qctx.tasty._; ... }`.
 */
trait QuoteContext { self =>

  /** Low-level Typed AST API `tasty` metaprogramming API.
   *  This API does not have the static type guarantiees that `Expr` and `Type` provide.
   */
  val tasty: scala.tasty.Reflection

  /** Type of a QuoteContext provided by a splice within a quote that took this context.
   *  It is only required if working with the reflection API.
   *
   *  Usually it is infered by the quotes an splices typing. But sometimes it is necessary
   *  to explicitly state that a context is nested as in the following example:
   *
   *  ```scala
   *  def run(using qctx: QuoteContext)(tree: qctx.tasty.Tree): Unit =
   *    def nested()(using qctx.Nested): Expr[Int] = '{  ${ makeExpr(tree) } + 1  }
   *    '{  ${ nested() } + 2 }
   *  def makeExpr(using qctx: QuoteContext)(tree: qctx.tasty.Tree): Expr[Int] = ???
   *  ```
   */
  type Nested = QuoteContext {
    val tasty: self.tasty.type
  }

}

object QuoteContext {
  // TODO remove in 0.26.0
  @deprecated("Errors and warnings have been moved to scala.quoted.Reporting", "0.25.0")
  given error_and_warining_on_QuoteContext as Conversion[QuoteContext, Reporting.type] = _ => Reporting
}
