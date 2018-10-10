package dotty.tools.languageserver.worksheet

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.languageserver.DottyLanguageServer

import org.eclipse.lsp4j.jsonrpc._//{CancelChecker, CompletableFutures}
import org.eclipse.lsp4j.jsonrpc.services._//{JsonSegment, JsonRequest}

import java.net.URI
import java.util.concurrent.{CompletableFuture, ConcurrentHashMap}

@JsonSegment("worksheet")
trait WorksheetService { thisServer: DottyLanguageServer =>

  val worksheets: ConcurrentHashMap[URI, CompletableFuture[_]] = new ConcurrentHashMap()

  @JsonRequest
  def run(params: WorksheetRunParams): CompletableFuture[WorksheetRunResult] = thisServer.synchronized {
    val uri = new URI(params.textDocument.getUri)
    val future =
      computeAsync { cancelChecker =>
        try {
          val driver = driverFor(uri)
          val sendMessage = (line: Int, msg: String) => client.publishOutput(WorksheetRunOutput(params.textDocument, line, msg))
          runWorksheet(driver, uri, sendMessage, cancelChecker)(driver.currentCtx)
          WorksheetRunResult(success = true)
        } catch {
          case _: Throwable =>
            WorksheetRunResult(success = false)
        } finally {
          worksheets.remove(uri)
        }
      }
    worksheets.put(uri, future)
    future
  }

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
                                sendMessage: (Int, String) => Unit,
                                cancelChecker: CancelChecker)(
      implicit ctx: Context): Unit = {
    val trees = driver.openedTrees(uri)
    trees.headOption.foreach { tree =>
      Worksheet.run(tree, sendMessage, cancelChecker)
    }
  }
}
