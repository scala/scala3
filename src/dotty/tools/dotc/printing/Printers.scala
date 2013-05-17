package dotty.tools.dotc
package printing

import core.Contexts.Context

trait Printers { this: Context =>

  /** A creation method for printers, depending on debug option */
  def printerFn = if (this.debug) plainPrinter else refinedPrinter

  /** A function creatung a printer */
  def printer = printerFn(this)

}

