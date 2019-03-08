package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.worksheet.{WorksheetRunOutput, WorksheetRunParams, WorksheetRunResult}
import dotty.tools.languageserver.util.embedded.CodeMarker

import java.net.URI
import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j.VersionedTextDocumentIdentifier

abstract class WorksheetAction extends Action {

  /** Triggers running the worksheet. */
  def triggerRun(marker: CodeMarker): Exec[CompletableFuture[WorksheetRunResult]] = {
    server.run(WorksheetRunParams(marker.toVersionedTextDocumentIdentifier))
  }

  /** The output of the worksheet that contains `marker`. */
  def worksheetOutput(marker: CodeMarker): Exec[List[WorksheetRunOutput]] = {
    val textDocument = marker.toVersionedTextDocumentIdentifier
    client.worksheetOutput.get.filter(_.textDocument.getUri == textDocument.getUri)
  }

}
