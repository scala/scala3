package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.embedded.CodeMarker

import org.eclipse.lsp4j.{DidSaveTextDocumentParams, MessageParams, MessageType}

abstract class WorksheetAction extends Action {

  def triggerEvaluation(marker: CodeMarker): Exec[Unit] = {
    val file = marker.toTextDocumentIdentifier
    server.didSave(new DidSaveTextDocumentParams(file))
  }

  def triggerCancellation(marker: CodeMarker): Exec[Unit] = {
    val file = {
      val file = marker.toTextDocumentIdentifier
      file.setUri(file.getUri.replaceFirst("file:", "cancel:"))
      file
    }
    server.didSave(new DidSaveTextDocumentParams(file))
  }

  def getLogs(marker: CodeMarker): Exec[List[String]] = {
    def matches(params: MessageParams): Boolean =
      params.getType == MessageType.Info && params.getMessage.startsWith(marker.file.uri)
    client.log.get.collect {
      case params: MessageParams if matches(params) =>
        params.getMessage.substring(marker.file.uri.length).trim
    }
  }
}
