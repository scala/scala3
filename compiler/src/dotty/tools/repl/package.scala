/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package dotty.tools

import dotc.core.Contexts.Context
import dotc.core.Symbols.Symbol
import dotc.printing.ReplPrinter
import dotc.reporting.{HideNonSensicalMessages, StoreReporter, UniqueMessagePositions}

package object repl {
  /** Create empty outer store reporter */
  private[repl] def newStoreReporter: StoreReporter =
    new StoreReporter(null)
    with UniqueMessagePositions with HideNonSensicalMessages

  private[repl] implicit class ShowUser(val s: Symbol) extends AnyVal {
    def showUser(implicit ctx: Context): String = {
      val printer = new ReplPrinter(ctx)
      val text = printer.dclText(s)
      text.mkString(ctx.settings.pageWidth.value, ctx.settings.printLines.value)
    }
  }
}
