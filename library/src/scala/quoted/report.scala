package scala.quoted

object report:

  /** Report an error at the position of the macro expansion */
  def error(msg: => String)(using Quotes): Unit =
    import qctx.reflect._
    Reporting.error(msg, Position.ofMacroExpansion)

  /** Report an error at the on the position of `expr` */
  def error(msg: => String, expr: Expr[Any])(using Quotes): Unit =
    import qctx.reflect._
    Reporting.error(msg, Term.of(expr).pos)

  /** Report an error at the position of the macro expansion and throws a StopMacroExpansion */
  def throwError(msg: => String)(using Quotes): Nothing = {
    error(msg)
    throw new runtime.StopMacroExpansion
  }
  /** Report an error at the on the position of `expr` and throws a StopMacroExpansion */
  def throwError(msg: => String, expr: Expr[Any])(using Quotes): Nothing = {
    error(msg, expr)
    throw new runtime.StopMacroExpansion
  }

  /** Report a warning */
  def warning(msg: => String)(using Quotes): Unit =
    import qctx.reflect._
    Reporting.warning(msg, Position.ofMacroExpansion)

  /** Report a warning at the on the position of `expr` */
  def warning(msg: => String, expr: Expr[Any])(using Quotes): Unit =
    import qctx.reflect._
    Reporting.warning(msg, Term.of(expr).pos)

end report
