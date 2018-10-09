package dotty.tools.languageserver.worksheet

import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification

/**
 * A `LanguageClient` that supports the `worksheet/publishOutput` notification.
 *
 * @see dotty.tools.languageserver.worksheet.WorksheetExecOutput
 */
trait WorksheetClient extends LanguageClient {
  @JsonNotification("worksheet/publishOutput")
  def publishOutput(output: WorksheetExecOutput): Unit
}

