package dotty.tools.languageserver.worksheet

import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification

/**
 * A `LanguageClient` that supports worksheet-specific notifications.
 */
trait WorksheetClient extends LanguageClient {
  /**
   *  A notification that tells the client that a line of a worksheet produced
   *  the specified output.
   */
  @JsonNotification("worksheet/publishOutput")
  def publishOutput(output: WorksheetExecOutput): Unit
}

