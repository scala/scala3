package dotty.tools
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

  override def isRunnable(using Context): Boolean = true

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] = {
    assert(units.size == 1) // REPl runs one compilation unit at a time
    val unit = units.head
    val unitContext = ctx.fresh.setCompilationUnit(unit)
    enterSyms(using unitContext)
    typeCheck(using unitContext)
    List(unit)
  }
}
