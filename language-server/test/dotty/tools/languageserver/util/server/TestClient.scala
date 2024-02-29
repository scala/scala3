package dotty.tools.languageserver.util.server

import dotty.tools.languageserver.worksheet.{WorksheetRunOutput}
import dotty.tools.languageserver.DottyClient


import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services._

import scala.collection.mutable.Buffer

class TestClient extends DottyClient {

  class Log[T] {
    private val log = Buffer.empty[T]

    def +=(elem: T): this.type = { log += elem; this }
    def get: List[T] = log.toList
    def clear(): Unit = log.clear()
  }

  val log = new Log[MessageParams]
  val diagnostics = new Log[PublishDiagnosticsParams]
  val telemetry = new Log[Any]
  val worksheetOutput = new Log[WorksheetRunOutput]
  val requests = new Log[(ShowMessageRequestParams, CompletableFuture[MessageActionItem])]

  override def logMessage(message: MessageParams) = {
    log += message
  }

  override def showMessage(messageParams: MessageParams) = {
    log += messageParams
  }

  override def telemetryEvent(obj: scala.Any) = {
    telemetry += obj
  }

  override def showMessageRequest(requestParams: ShowMessageRequestParams) = {
    val reply = new CompletableFuture[MessageActionItem]
    requests += ((requestParams, reply))
    reply
  }

  override def publishDiagnostics(diagnosticsParams: PublishDiagnosticsParams) = {
    diagnostics += diagnosticsParams
  }

  override def publishOutput(output: WorksheetRunOutput) = {
    worksheetOutput += output
  }

}
