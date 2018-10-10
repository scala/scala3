package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.PositionContext
import dotty.tools.languageserver.util.embedded.CodeMarker

import java.util.concurrent.TimeUnit

import org.junit.Assert.{assertEquals, assertTrue, fail}

class WorksheetRun(marker: CodeMarker, expected: Seq[String], strict: Boolean) extends WorksheetAction {

  override def execute(): Exec[Unit] = {
    val result = triggerRun(marker).get(30, TimeUnit.SECONDS)
    assertTrue(result.success)

    val logs = worksheetOutput(marker).map(out => s"${out.line}:${out.content}")

    if (strict) {
      assertEquals(expected, logs)
    } else {
      expected.zip(logs).foreach {
        case (expected, message) => assertTrue(s"'$message' didn't start with '$expected'", message.startsWith(expected))
      }
    }
    client.worksheetOutput.clear()
  }

  override def show: PositionContext.PosCtx[String] =
    s"WorksheetRun(${marker.file}, ${expected})"
}
