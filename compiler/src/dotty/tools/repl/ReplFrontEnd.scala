/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package dotty.tools
package repl

import dotc.typer.FrontEnd
import dotc.CompilationUnit
import dotc.core.Contexts.Context

/** A customized `FrontEnd` for the REPL
 *
 *  This customized front end does not perform parsing as part of its `runOn`
 *  method. This allows us to keep the parsing separate from the rest of the
 *  compiler pipeline.
 */
private[repl] class REPLFrontEnd extends FrontEnd {

  override def isRunnable(implicit ctx: Context): Boolean = true

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
    assert(units.size == 1) // REPl runs one compilation unit at a time

    val unitContext = ctx.fresh.setCompilationUnit(units.head)
    enterSyms(unitContext)
    typeCheck(unitContext)
    List(unitContext.compilationUnit)
  }
}
