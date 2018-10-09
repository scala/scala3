package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.worksheet.{WorksheetExecOutput, WorksheetExecParams, WorksheetExecResult}
import dotty.tools.languageserver.util.embedded.CodeMarker

import java.net.URI
import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j.VersionedTextDocumentIdentifier

abstract class WorksheetAction extends Action {

  /** Triggers the evaluation of the worksheet. */
  def triggerEvaluation(marker: CodeMarker): Exec[CompletableFuture[WorksheetExecResult]] = {
    server.exec(WorksheetExecParams(marker.toVersionedTextDocumentIdentifier))
  }

  /** The output of the worksheet that contains `marker`. */
  def worksheetOutput(marker: CodeMarker): Exec[List[WorksheetExecOutput]] = {
    val textDocument = marker.toVersionedTextDocumentIdentifier
    client.worksheetOutput.get.filter(_.textDocument.getUri == textDocument.getUri)
  }

}
