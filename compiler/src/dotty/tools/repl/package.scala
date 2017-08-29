package dotty.tools

import dotc.core.Contexts.Context
import dotc.core.Symbols.Symbol
import dotc.core.Denotations.Denotation
import dotc.reporting.diagnostic.MessageContainer
import dotc.printing.UserFacingPrinter

import dotc.reporting.{
  StoreReporter,
  UniqueMessagePositions,
  HideNonSensicalMessages
}

package object repl {
  /** Create empty outer store reporter */
  private[repl] def storeReporter: StoreReporter =
    new StoreReporter(null)
    with UniqueMessagePositions with HideNonSensicalMessages

  private[repl] implicit class ShowUser(val s: Symbol) extends AnyVal {
    def showUser(implicit ctx: Context): String = {
      val printer = new UserFacingPrinter(ctx)
      val text = printer.dclText(s)
      text.mkString(ctx.settings.pageWidth.value)
    }
  }

  private[repl] implicit class StoreReporterContext(val ctx: Context) extends AnyVal {
    def flushBufferedMessages(): List[MessageContainer] =
      ctx.reporter match {
        case rep: StoreReporter => rep.removeBufferedMessages(ctx)
        case _ => Nil
      }
  }
}
