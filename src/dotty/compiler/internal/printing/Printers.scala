package dotty.compiler
package internal
package printing

import core.Contexts.Context

trait Printers { this: Context =>

  /** A function creating a printer */
  def printer = {
    val pr = printerFn(this)
    if (this.debug) pr.plain else pr
  }
}

