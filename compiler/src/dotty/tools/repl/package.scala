package dotty.tools

import dotc.core.Contexts.ContextRenamed
import dotc.core.Symbols.Symbol
import dotc.printing.ReplPrinter
import dotc.reporting.{HideNonSensicalMessages, StoreReporter, UniqueMessagePositions}

package object repl {
  /** Create empty outer store reporter */
  private[repl] def newStoreReporter: StoreReporter =
    new StoreReporter(null)
    with UniqueMessagePositions with HideNonSensicalMessages

  private[repl] implicit class ShowUser(val s: Symbol) extends AnyVal {
    def showUser(implicit ctx: ContextRenamed): String = {
      val printer = new ReplPrinter(ctx)
      val text = printer.dclText(s)
      text.mkString(ctx.settings.pageWidth.value, ctx.settings.printLines.value)
    }
  }
}
