package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.{CodeRange, PositionContext, SymInfo}
import org.eclipse.lsp4j._

import scala.collection.JavaConverters._

/**
 * An action requesting for all the symbols in the workspace matching `query`.
 * This action corresponds to the `workspace/symbol` method of the Language Server Protocol.
 *
 * @param query    The string to query for.
 * @param expected The expected results.
 */
class CodeSymbol(query: String, symbols: Seq[SymInfo]) extends Action {

  override def execute(): Exec[Unit] = {
    val results = server.symbol(new WorkspaceSymbolParams(query)).get()
    assert(results.size() == symbols.size, results)
    for ((symInfo, expected) <- results.asScala.zip(symbols)) {
      assert(symInfo == expected.toSymInformation, results)
    }
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeDocumentSymbol($query, ${symbols.map(_.show)})"
}
