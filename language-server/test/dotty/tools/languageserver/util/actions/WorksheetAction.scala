package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.worksheet.{WorksheetExecOutput, WorksheetExecParams, WorksheetExecResponse}
import dotty.tools.languageserver.util.embedded.CodeMarker

import java.net.URI
import java.util.concurrent.CompletableFuture

abstract class WorksheetAction extends Action {

  /** Get the URI of the worksheet that contains `marker`. */
  def getUri(marker: CodeMarker): Exec[URI] = new URI(marker.toTextDocumentIdentifier.getUri)

  /** Triggers the evaluation of the worksheet. */
  def triggerEvaluation(marker: CodeMarker): Exec[CompletableFuture[WorksheetExecResponse]] = {
    val uri = getUri(marker)
    server.exec(WorksheetExecParams(uri))
  }

  /** The output of the worksheet that contains `marker`. */
  def worksheetOutput(marker: CodeMarker): Exec[List[WorksheetExecOutput]] = {
    val uri = getUri(marker)
    client.worksheetOutput.get.filter(_.uri == uri)
  }

}
