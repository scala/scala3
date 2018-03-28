package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.{PositionContext, SymInfo}

import scala.collection.JavaConverters._

/**
 * An action requesting for the symbols found in the document matching `marker`.
 * This action corresponds to the `textDocument/documentSymbol` method of the Language Server
 * Protocol.
 *
 * @param marker The marker that identifies the document for which to request the symbols.
 * @param expected The expected symbols to receive.
 */
class CodeDocumentSymbol(override val marker: CodeMarker, expected: Seq[SymInfo]) extends ActionOnMarker {

  override def execute(): Exec[Unit] = {
    val results = server.documentSymbol(marker.toDocumentSymbolParams).get()
    assert(results.size == expected.size, results)
    for ((symInfo, expected) <- results.asScala.zip(expected)) {
      assert(symInfo == expected.toSymInformation, results)
    }
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeDocumentSymbol(${marker.show}, ${expected.map(_.show)})"
}
