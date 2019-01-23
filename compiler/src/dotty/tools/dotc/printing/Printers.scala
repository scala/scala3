/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package dotty.tools.dotc
package printing

import core.Contexts.Context

trait Printers { this: Context =>

  /** A function creating a printer */
  def printer: Printer = {
    val pr = printerFn(this)
    if (this.settings.YplainPrinter.value) pr.plain else pr
  }
}

