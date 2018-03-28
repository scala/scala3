package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util._
import dotty.tools.languageserver.util.embedded.CodeMarker

import scala.collection.JavaConverters._

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
    results.asScala.zip(expected).foreach {
      case (result, expected) =>
        assert(result == expected.toLocation, s"Expected ${expected.toLocation}, found $result.")
    }
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeDefinition(${range.show}, ${expected.map(_.show)})"
}
