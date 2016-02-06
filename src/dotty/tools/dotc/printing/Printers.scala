package dotty.tools.dotc
package printing

import core.Contexts.Context

trait Printers { this: Context =>

  /** A function creating a printer */
  def printer = {
    val pr = printerFn(this)
    if (this.settings.YplainPrinter.value) pr.plain else pr
  }
}

