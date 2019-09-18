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

  def show(expr: Expr[_], syntaxHighlight: SyntaxHighlight): String = {
    import tasty._
    expr.unseal.show(syntaxHighlight)
  }

  def show(tpe: Type[_], syntaxHighlight: SyntaxHighlight): String = {
    import tasty._
    tpe.unseal.show(syntaxHighlight)
  }

  /** Report an error */
  def error(msg: => String): Unit = {
    import tasty._
    tasty.error(msg, rootPosition)(given rootContext)
  }

  /** Report an error at the on the position of `expr` */
  def error(msg: => String, expr: Expr[_]): Unit = {
    import tasty._
    tasty.error(msg, expr.unseal.pos)(given rootContext)
  }

  /** Report a warning */
  def warning(msg: => String): Unit = {
    import tasty._
    tasty.warning(msg, rootPosition)(given rootContext)
  }

  /** Report a warning at the on the position of `expr` */
  def warning(msg: => String, expr: Expr[_]): Unit = {
    import tasty._
    tasty.warning(msg, expr.unseal.pos)(given rootContext)
  }

}

object QuoteContext {
  def macroContext: QuoteContext = throw new Exception("Not in inline macro.")
}
