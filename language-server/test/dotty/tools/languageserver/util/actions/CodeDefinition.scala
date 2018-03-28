package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util._
import dotty.tools.languageserver.util.embedded.CodeMarker

class CodeDefinition(val range: CodeRange, expected: Seq[CodeRange]) extends ActionOnRange {

  override def onMarker(marker: CodeMarker): Exec[Unit] = {
    val results = server.definition(fix(marker.toTextDocumentPositionParams)).get()
    assert(results.size == expected.size, s"Expected ${expected.size} matches, found ${results.size}")
    (0 until results.size).foreach { i =>
      assert(results.get(i) == expected(i).toLocation, s"Expected ${expected(i).toLocation}, found ${results.get(i)}.")
    }
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeDefinition(${range.show}, ${expected.map(_.show)})"
}
