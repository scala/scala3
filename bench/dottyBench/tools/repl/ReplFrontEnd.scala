package dottyBench.tools
package repl

import dotc.typer.FrontEnd
import dotc.CompilationUnit
import dotc.core.Contexts._

/** A customized `FrontEnd` for the REPL
 *
 *  This customized front end does not perform parsing as part of its `runOn`
 *  method. This allows us to keep the parsing separate from the rest of the
 *  compiler pipeline.
 */
private[repl] class REPLFrontEnd extends FrontEnd {

  override def isRunnable(using Ctx): Boolean = true

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] = {
    given CState = currentContext.cstate
    assert(units.size == 1) // REPl runs one compilation unit at a time

    val unitContext = ctx.fresh.setCompilationUnit(units.head)
    enterSyms(using unitContext)
    typeCheck(using unitContext)
    List(unitContext.compilationUnit)
  }
}
