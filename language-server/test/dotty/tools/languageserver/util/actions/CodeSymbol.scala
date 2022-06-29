package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.{PositionContext, SymInfo}
import org.eclipse.lsp4j.WorkspaceSymbolParams
import org.junit.Assert.assertEquals

import scala.jdk.CollectionConverters._

/**
 * An action requesting for all the symbols in the workspace matching `query`.
 * This action corresponds to the `workspace/symbol` method of the Language Server Protocol.
 *
 * @param query    The string to query for.
 * @param expected The expected results.
 */
class CodeSymbol(query: String, expected: Seq[SymInfo]) extends Action {

  override def execute(): Exec[Unit] = {
    val results = server.symbol(new WorkspaceSymbolParams(query)).get().asScala
    val expectedSymInfo = expected.map(_.toSymInformation)

    assertEquals(expectedSymInfo, results)
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeDocumentSymbol($query, ${expected.map(_.show)})"
}
