package scala.quoted

object report:

  /** Report an error at the position of the macro expansion */
  def error(msg: => String)(using qctx: QuoteContext): Unit =
    import qctx.reflect._
    Reporting.error(msg, Position.ofMacroExpansion)

  /** Report an error at the on the position of `expr` */
  def error(msg: => String, expr: Expr[Any])(using qctx: QuoteContext): Unit =
    import qctx.reflect._
    Reporting.error(msg, Term.of(expr).pos)

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
    import qctx.reflect._
    Reporting.warning(msg, Position.ofMacroExpansion)

  /** Report a warning at the on the position of `expr` */
  def warning(msg: => String, expr: Expr[Any])(using qctx: QuoteContext): Unit =
    import qctx.reflect._
    Reporting.warning(msg, Term.of(expr).pos)

  /** Throwable used to stop the expansion of a macro after an error was reported */
  class StopQuotedContext extends Throwable

end report
