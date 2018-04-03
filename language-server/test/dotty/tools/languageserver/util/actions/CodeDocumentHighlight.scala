package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.{CodeRange, PositionContext}
import org.eclipse.lsp4j._

import scala.collection.JavaConverters._

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
    val (references, kinds) = expected.unzip
    val results = server.documentHighlight(fix(marker.toTextDocumentPositionParams)).get()
    assert(results.size() == references.size, results)
    assert(references.size == kinds.length, results)
    results.asScala.zip(references).zip(kinds).foreach { case ((dhl, ref), kind) =>
      assert(dhl.getKind == kind, results)
      assert(dhl.getRange == ref.toRange, results)
    }
  }

  override def show: PositionContext.PosCtx[String] = {
    val (references, kinds) = expected.unzip
    s"CodeDocumentHighlight(${range.show}, ${references.map(_.show)}, $kinds)"
  }
}
