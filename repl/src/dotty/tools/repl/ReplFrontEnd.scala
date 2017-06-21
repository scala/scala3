package dotty.tools
package repl

import dotc.typer.FrontEnd
import dotc.CompilationUnit
import dotc.core.Contexts.Context

class REPLFrontEnd extends FrontEnd {
  override def phaseName = "replFrontEnd"

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
