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
  override def phaseName = "replFrontEnd"

  override def isRunnable(implicit ctx: Context) = true

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context) = {
    val unitContexts = for (unit <- units) yield ctx.fresh.setCompilationUnit(unit)
    var remaining = unitContexts
    while (remaining.nonEmpty) {
      enterSyms(remaining.head)
      remaining = remaining.tail
    }
    unitContexts.foreach(enterAnnotations(_))
    unitContexts.foreach(typeCheck(_))
    unitContexts.map(_.compilationUnit).filterNot(discardAfterTyper)
  }
}
