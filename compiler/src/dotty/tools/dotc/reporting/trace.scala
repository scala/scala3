package dotty.tools
package dotc
package reporting

import core.Contexts.Context
import config.Config
import config.Printers
import core.Mode

object trace {

  @inline
  def onDebug[TD](question: => String)(op: => TD)(implicit ctx: Context): TD =
    conditionally(ctx.settings.debugTrace.value, question, false)(op)

  @inline
  def conditionally[TC](cond: Boolean, question: => String, show: Boolean)(op: => TC)(implicit ctx: Context): TC =
    if (Config.tracingEnabled && cond) apply[TC](question, Printers.default, show)(op)
    else op

  @inline
  def apply[T](question: => String, printer: Printers.Printer, show: Boolean)(op: => T)(implicit ctx: Context): T =
    if (!Config.tracingEnabled || printer.eq(config.Printers.noPrinter)) op
    else doTrace[T](question, printer, show)(op)

  @inline
  def apply[T](question: => String, printer: Printers.Printer)(op: => T)(implicit ctx: Context): T =
    apply[T](question, printer, false)(op)

  @inline
  def apply[T](question: => String, show: Boolean)(op: => T)(implicit ctx: Context): T =
    apply[T](question, Printers.default, show)(op)

  @inline
  def apply[T](question: => String)(op: => T)(implicit ctx: Context): T =
    apply[T](question, Printers.default, false)(op)

  private def doTrace[T](question: => String, printer: Printers.Printer = Printers.default, show: Boolean = false)
                                (op: => T)(implicit ctx: Context): T = {
    def resStr(res: Any): String = res match {
      case res: printing.Showable if show => res.show
      case _ => String.valueOf(res)
    }
    // Avoid evaluating question multiple time, since each evaluation
    // may cause some extra logging output.
    lazy val q: String = question
    apply[T](s"==> $q?", (res: Any) => s"<== $q = ${resStr(res)}")(op)
  }

  def apply[T](leading: => String, trailing: Any => String)(op: => T)(implicit ctx: Context): T =
    if (ctx.mode.is(Mode.Printing)) op
    else {
      var finalized = false
      var logctx = ctx
      while (logctx.reporter.isInstanceOf[StoreReporter]) logctx = logctx.outer
      def finalize(result: Any, note: String) =
        if (!finalized) {
          ctx.base.indent -= 1
          logctx.log(s"${ctx.base.indentTab * ctx.base.indent}${trailing(result)}$note")
          finalized = true
        }
    try {
      logctx.log(s"${ctx.base.indentTab * ctx.base.indent}$leading")
      ctx.base.indent += 1
      val res = op
      finalize(res, "")
      res
    } catch {
      case ex: Throwable =>
        finalize("<missing>", s" (with exception $ex)")
        throw ex
    }
  }
}