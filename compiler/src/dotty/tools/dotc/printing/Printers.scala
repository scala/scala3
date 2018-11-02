package dotty.tools.dotc
package printing

import core.Contexts.ContextRenamed

trait Printers { this: ContextRenamed =>

  /** A function creating a printer */
  def printer: Printer = {
    val pr = printerFn(this)
    if (this.settings.YplainPrinter.value) pr.plain else pr
  }
}

