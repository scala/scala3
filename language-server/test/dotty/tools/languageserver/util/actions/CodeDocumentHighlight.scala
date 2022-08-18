package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.{CodeRange, PositionContext}
import org.eclipse.lsp4j.DocumentHighlightKind
import org.junit.Assert.assertEquals

import scala.jdk.CollectionConverters._

/**
 * An action requesting for the ranges that should be highlighted, when a position within `range`
 * is selected.
 * This action corresponds to the `textDocument/documentHighlight` method of the Language Server
 * Protocol.
 *
 * @param range    The range to of positions to test.
 * @param expected The expected results.
 */
class CodeDocumentHighlight(override val range: CodeRange,
                            expected: Seq[(CodeRange, DocumentHighlightKind)]) extends ActionOnRange {

  override def onMarker(marker: CodeMarker): Exec[Unit] = {
    val expectedPairs = expected.map { case (codeRange, kind) => (codeRange.toRange, kind) }.sorted
    val results = server.documentHighlight(marker.toTextDocumentPositionParams).get()
    val resultPairs = results.asScala.map { result => (result.getRange, result.getKind) }.sorted

    assertEquals(expectedPairs, resultPairs)
  }

  override def show: PositionContext.PosCtx[String] = {
    val (references, kinds) = expected.unzip
    s"CodeDocumentHighlight(${range.show}, ${references.map(_.show)}, $kinds)"
  }
}
