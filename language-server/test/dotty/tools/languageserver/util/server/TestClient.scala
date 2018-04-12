package dotty.tools.languageserver.util.server

import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services._

class TestClient extends LanguageClient {

  private val log = new StringBuilder

  def getLog: String = log.result()

  override def logMessage(message: MessageParams) = {
    log.append(message.toString)
  }

  override def showMessage(messageParams: MessageParams) = {
    log.append(messageParams.toString)
  }

  override def telemetryEvent(obj: scala.Any) = {
    log.append(obj.toString)
  }

  override def showMessageRequest(requestParams: ShowMessageRequestParams) = {
    log.append(requestParams.toString)
    new CompletableFuture[MessageActionItem]
  }

  override def publishDiagnostics(diagnostics: PublishDiagnosticsParams) = {
    log.append(diagnostics.toString)
  }

}
