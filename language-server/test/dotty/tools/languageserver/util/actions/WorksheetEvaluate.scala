package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.{PositionContext}
import dotty.tools.languageserver.util.embedded.CodeMarker

import org.eclipse.lsp4j.{DidSaveTextDocumentParams, MessageParams, MessageType}

import org.junit.Assert.assertEquals

class WorksheetEvaluate(marker: CodeMarker, expected: Seq[String]) extends Action {
  override def execute(): Exec[Unit] = {
    val file = marker.toTextDocumentIdentifier
    server.didSave(new DidSaveTextDocumentParams(file))

    while (!getLogs(marker).contains("FINISHED")) Thread.sleep(100)

    assertEquals(expected, getLogs(marker).init)
    client.log.clear()
  }

  override def show: PositionContext.PosCtx[String] =
    s"WorksheetEvaluate(${marker.file}, ${expected})"

  private def getLogs(marker: CodeMarker): Exec[List[String]] = {
    def matches(params: MessageParams): Boolean =
      params.getType == MessageType.Info && params.getMessage.startsWith(marker.file.uri)
    client.log.get.collect {
      case params: MessageParams if matches(params) =>
        params.getMessage.substring(marker.file.uri.length).trim
    }
  }
}
