package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.{CodeRange, PositionContext, SymInfo}
import org.eclipse.lsp4j._

import scala.collection.JavaConverters._

class CodeSymbol(query: String, symbols: Seq[SymInfo]) extends Action {

  override def execute(): Exec[Unit] = {
    val res = server.symbol(new WorkspaceSymbolParams(query)).get()
    assert(res.size() == symbols.size, res)
    for ((symInfo, expected) <- res.asScala.zip(symbols)) {
      assert(symInfo == expected.toSymInformation, res)
    }
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeDocumentSymbol($query, ${symbols.map(_.show)})"
}
