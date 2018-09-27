package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.PositionContext
import dotty.tools.languageserver.util.embedded.CodeMarker

import org.junit.Assert.{assertEquals, fail}

class WorksheetCancel(marker: CodeMarker, afterMs: Long) extends WorksheetAction {

  private final val cancellationTimeoutMs = 10 * 1000

  override def execute(): Exec[Unit] = {
    triggerEvaluation(marker)
    Thread.sleep(afterMs)
    triggerCancellation(marker)

    val timeAtCancellation = System.currentTimeMillis()
    while (!getLogs(marker).contains("FINISHED")) {
      if (System.currentTimeMillis() - timeAtCancellation > cancellationTimeoutMs) {
        fail(s"Couldn't cancel worksheet evaluation after ${cancellationTimeoutMs} ms.")
      }
      Thread.sleep(100)
    }

    client.log.clear()
  }

  override def show: PositionContext.PosCtx[String] =
    s"WorksheetCancel(${marker.file}, ${afterMs})"
}
