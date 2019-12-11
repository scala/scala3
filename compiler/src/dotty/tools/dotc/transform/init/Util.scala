package dotty.tools.dotc
package transform
package init

import core.Contexts._
import config.Printers.Printer


object Util {
  def traceIndented(msg: String, printer: Printer)(implicit ctx: Context): Unit =
    printer.println(s"${ctx.base.indentTab * ctx.base.indent} $msg")

  def traceOp(msg: String, printer: Printer)(op: => Unit)(implicit ctx: Context): Unit = {
    traceIndented(s"==> ${msg}", printer)
    op
    traceIndented(s"<== ${msg}", printer)
  }
}