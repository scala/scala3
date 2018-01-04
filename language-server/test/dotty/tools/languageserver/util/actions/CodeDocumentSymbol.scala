package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.{PositionContext, SymInfo}

import scala.collection.JavaConverters._

class CodeDocumentSymbol(val marker: CodeMarker, symbols: Seq[SymInfo]) extends ActionOnMarker {

  override def execute(): Exec[Unit] = {
    val res = server.documentSymbol(marker.toDocumentSymbolParams).get()
    assert(res.size() == symbols.size, res)
    for ((symInfo, expected) <- res.asScala.zip(symbols)) {
      assert(symInfo == expected.toSymInformation, res)
    }
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeDocumentSymbol(${marker.show}, ${symbols.map(_.show)})"
}
