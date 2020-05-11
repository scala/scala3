package scala.quoted

object report:

  /** Report an error at the position of the macro expansion */
  def error(using s: Scope)(msg: => String): Unit =
    scope.tasty.error(msg, scope.tasty.rootPosition)

  /** Report an error at the on the position of `expr` */
  def errorOn(using s: Scope)(expr: s.Expr[Any], msg: => String): Unit =
    scope.tasty.error(msg, expr.pos)

  /** Report an error at the position of the macro expansion and throws a StopQuoteExpansion */
  def throwError(msg: => String)(using Scope): Nothing = {
    error(msg)
    throw new StopQuoteExpansion
  }
  /** Report an error at the on the position of `expr` and throws a StopQuoteExpansion */
  def throwErrorOn(using s: Scope)(expr: s.Expr[Any], msg: => String): Nothing = {
    errorOn(expr, msg)
    throw new StopQuoteExpansion
  }

  /** Report a warning */
  def warning(using s: Scope)(msg: => String): Unit =
    s.tasty.warning(msg, s.tasty.rootPosition)

  /** Report a warning at the on the position of `expr` */
  def warningOn(using s: Scope)(expr: s.Expr[Any], msg: => String): Unit =
    scope.tasty.warning(msg, expr.pos)

end report
