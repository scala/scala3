package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.{CodeRange, PositionContext}
import dotty.tools.languageserver.util.embedded.CodeMarker

import scala.jdk.CollectionConverters._

import org.junit.Assert.assertEquals

/**
 * An action requesting for the definition of the symbol inside `range`.
 * This action corresponds to the `textDocument/definition` method of the Language Server Protocol.
 *
 * @param range    The range of positions for which to request the definition.
 * @param expected The expected results.
 */
class CodeDefinition(override val range: CodeRange, expected: Seq[CodeRange]) extends ActionOnRange {

  override def onMarker(marker: CodeMarker): Exec[Unit] = {
    val results = server.definition(marker.toTextDocumentPositionParams).get().asScala.toSeq.sorted
    val expectedLocations = expected.map(_.toLocation).sorted

    assertEquals(expectedLocations, results)
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeDefinition(${range.show}, ${expected.map(_.show)})"
}
