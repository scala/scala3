package dotty.tools.languageserver.worksheet

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.languageserver.DottyLanguageServer
import dotty.tools.languageserver.DottyLanguageServer.range

import org.eclipse.lsp4j.jsonrpc._//{CancelChecker, CompletableFutures}
import org.eclipse.lsp4j.jsonrpc.services._//{JsonSegment, JsonRequest}

import java.net.URI
import java.util.concurrent.{CompletableFuture, ConcurrentHashMap}

@JsonSegment("worksheet")
trait WorksheetService { thisServer: DottyLanguageServer =>

  @JsonRequest
  def run(params: WorksheetRunParams): CompletableFuture[WorksheetRunResult] =
    computeAsync(synchronize = false, fun = { cancelChecker =>
      val uri = new URI(params.textDocument.getUri)
      try {
        val driver = driverFor(uri)
        val sendMessage = (pos: SourcePosition, msg: String) =>
          client.publishOutput(WorksheetRunOutput(params.textDocument, range(pos).get, msg))

        runWorksheet(driver, uri, sendMessage, cancelChecker)(driver.currentCtx)
        cancelChecker.checkCanceled()
        WorksheetRunResult(success = true)
      } catch {
        case _: Throwable =>
          WorksheetRunResult(success = false)
      }
    })

  /**
   * Run the worksheet at `uri`.
   *
   * @param driver        The driver for the project that contains the worksheet.
   * @param uri           The URI of the worksheet.
   * @param sendMessage   A mean of communicating the results of evaluation back.
   * @param cancelChecker Token to check whether evaluation was cancelled
   */
  private def runWorksheet(driver: InteractiveDriver,
                           uri: URI,
                           sendMessage: (SourcePosition, String) => Unit,
                           cancelChecker: CancelChecker)(
      implicit ctx: Context): Unit = {
    val treeOpt = thisServer.synchronized {
      driver.openedTrees(uri).headOption
    }
    treeOpt.foreach(tree => Worksheet.run(tree, thisServer, sendMessage, cancelChecker))
  }
}
