package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util._
import dotty.tools.languageserver.util.embedded.CodeMarker

/**
 * An action requesting for the definition of the symbol inside `range`.
 * This action corresponds to the `textDocument/definition` method of the Language Server Protocol.
 *
 * @param range    The range of positions for which to request the definition.
 * @param expected The expected results.
 */
class CodeDefinition(override val range: CodeRange, expected: Seq[CodeRange]) extends ActionOnRange {

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
