package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.PositionContext
import dotty.tools.languageserver.util.embedded.CodeMarker

import org.junit.Assert.assertTrue

import java.util.concurrent.TimeUnit

class WorksheetCancel(marker: CodeMarker, afterMs: Long) extends WorksheetAction {

  override def execute(): Exec[Unit] = {
    val futureResult = triggerRun(marker)
    Thread.sleep(afterMs)
    val cancelled = futureResult.cancel(true)

    assertTrue(cancelled)

    client.worksheetOutput.clear()
  }

  override def show: PositionContext.PosCtx[String] =
    s"WorksheetCancel(${marker.file}, ${afterMs})"
}
