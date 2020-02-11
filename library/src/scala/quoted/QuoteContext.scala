package scala.quoted

import scala.quoted.show.SyntaxHighlight

/** Quotation context provided by a macro expansion or in the scope of `scala.quoted.run`.
 *  Used to perform all operations on quoted `Expr` or `Type`.
 *
 *  It contains the low-level Typed AST API `tasty` meta-programming API.
 *  This API does not have the static type guarantiees that `Expr` and `Type` provide.
 *
 *  @param tasty Typed AST API. Usage: `def f(qctx: QuoteContext) = { import qctx.tasty.{_, given _}; ... }`.
 */
class QuoteContext(val tasty: scala.tasty.Reflection) {

  def show(expr: Expr[_], syntaxHighlight: SyntaxHighlight): String = {
    import tasty.{_, given _}
    expr.unseal.showWith(syntaxHighlight)
  }

  def show(tpe: Type[_], syntaxHighlight: SyntaxHighlight): String = {
    import tasty.{_, given _}
    tpe.unseal.showWith(syntaxHighlight)
  }

  /** Report an error at the position of the macro expansion */
  def error(msg: => String): Unit = {
    import tasty.{_, given _}
    tasty.error(msg, rootPosition)
  }

  /** Report an error at the on the position of `expr` */
  def error(msg: => String, expr: Expr[Any]): Unit = {
    import tasty.{_, given _}
    tasty.error(msg, expr.unseal.pos)
  }

  /** Report an error at the position of the macro expansion and throws a StopQuotedContext */
  def throwError(msg: => String): Nothing = {
    error(msg)
    throw new StopQuotedContext
  }
  /** Report an error at the on the position of `expr` and throws a StopQuotedContext */
  def throwError(msg: => String, expr: Expr[Any]): Nothing = {
    error(msg, expr)
    throw new StopQuotedContext
  }

  /** Report a warning */
  def warning(msg: => String): Unit = {
    import tasty.{_, given _}
    tasty.warning(msg, rootPosition)
  }

  /** Report a warning at the on the position of `expr` */
  def warning(msg: => String, expr: Expr[_]): Unit = {
    import tasty.{_, given _}
    tasty.warning(msg, expr.unseal.pos)
  }

}
