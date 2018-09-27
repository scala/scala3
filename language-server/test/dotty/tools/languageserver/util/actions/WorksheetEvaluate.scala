package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.PositionContext
import dotty.tools.languageserver.util.embedded.CodeMarker

import org.junit.Assert.{assertEquals, fail}

class WorksheetEvaluate(marker: CodeMarker, expected: Seq[String]) extends WorksheetAction {

  private final val evaluationTimeoutMs = 30 * 1000

  override def execute(): Exec[Unit] = {
    triggerEvaluation(marker)

    val timeAtEvaluation = System.currentTimeMillis()
    while (!getLogs(marker).contains("FINISHED")) {
      if (System.currentTimeMillis() - timeAtEvaluation > evaluationTimeoutMs) {
        fail(s"Evaluation didn't finish after ${evaluationTimeoutMs} ms.")
      }
      Thread.sleep(100)
    }

    assertEquals(expected, getLogs(marker).init)
    client.log.clear()
  }

  override def show: PositionContext.PosCtx[String] =
    s"WorksheetEvaluate(${marker.file}, ${expected})"
}
