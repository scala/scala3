package scala.quoted

object report:

  /** Report an error at the position of the macro expansion */
  def error(msg: => String)(using qctx: QuoteContext): Unit =
    qctx.reflect.Reporting.error(msg, qctx.reflect.Position.ofMacroExpansion)

  /** Report an error at the on the position of `expr` */
  def error(msg: => String, expr: Expr[Any])(using qctx: QuoteContext): Unit =
    qctx.reflect.Reporting.error(msg, expr.unseal.pos)

  /** Report an error at the position of the macro expansion and throws a StopQuotedContext */
  def throwError(msg: => String)(using qctx: QuoteContext): Nothing = {
    error(msg)
    throw new StopQuotedContext
  }
  /** Report an error at the on the position of `expr` and throws a StopQuotedContext */
  def throwError(msg: => String, expr: Expr[Any])(using qctx: QuoteContext): Nothing = {
    error(msg, expr)
    throw new StopQuotedContext
  }

  /** Report a warning */
  def warning(msg: => String)(using qctx: QuoteContext): Unit =
    qctx.reflect.Reporting.warning(msg, qctx.reflect.Position.ofMacroExpansion)

  /** Report a warning at the on the position of `expr` */
  def warning(msg: => String, expr: Expr[_])(using qctx: QuoteContext): Unit =
    qctx.reflect.Reporting.warning(msg, expr.unseal.pos)

  /** Throwable used to stop the expansion of a macro after an error was reported */
  class StopQuotedContext extends Throwable

end report
