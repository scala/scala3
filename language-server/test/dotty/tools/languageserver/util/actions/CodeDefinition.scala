package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util._
import dotty.tools.languageserver.util.embedded.CodeMarker

class CodeDefinition(val range: CodeRange, refOpt: Option[CodeRange]) extends ActionOnRange {

  override def onMarker(marker: CodeMarker): Exec[Unit] = {
    val res = server.definition(fix(marker.toTextDocumentPositionParams)).get()
    assert(res.size() == refOpt.size, res)
    refOpt.foreach(ref => assert(res.get(0) == ref.toLocation, res))
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeDefinition(${range.show}, ${refOpt.map(_.show)})"
}
