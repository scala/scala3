package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.PositionContext
import dotty.tools.languageserver.util.embedded.CodeMarker

import org.junit.Assert.{assertEquals, assertTrue, fail}

class WorksheetEvaluate(marker: CodeMarker, expected: Seq[String], strict: Boolean) extends WorksheetAction {

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

    if (strict) {
      assertEquals(expected, getLogs(marker).init)
    } else {
      expected.zip(getLogs(marker).init).foreach {
        case (expected, message) => assertTrue(s"'$message' didn't start with '$expected'", message.startsWith(expected))
      }
    }
    client.log.clear()
  }

  override def show: PositionContext.PosCtx[String] =
    s"WorksheetEvaluate(${marker.file}, ${expected})"
}
