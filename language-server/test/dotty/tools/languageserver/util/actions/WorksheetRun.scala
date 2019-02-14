package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.{ CodeRange, PositionContext }
import dotty.tools.languageserver.util.embedded.CodeMarker

import java.util.concurrent.TimeUnit

import org.junit.Assert.{assertEquals, assertTrue, fail}

class WorksheetRun(marker: CodeMarker, expected: Seq[(CodeRange, String)], strict: Boolean) extends WorksheetAction {

  override def execute(): Exec[Unit] = {
    val result = triggerRun(marker).get(30, TimeUnit.SECONDS)
    assertTrue(result.success)

    val logs = worksheetOutput(marker).map(out => (out.range, out.content))

    expected.zip(logs).foreach {
      case ((expectedCodeRange, expectedOutput), (actualRange, actualOutput)) =>
        assertEquals(expectedCodeRange.toRange, actualRange)
        if (strict)
          assertEquals(expectedOutput, actualOutput)
        else
          assertTrue(s"'$actualOutput' didn't start with '$expectedOutput'", actualOutput.startsWith(expectedOutput))
    }
    client.worksheetOutput.clear()
  }

  override def show: PositionContext.PosCtx[String] =
    s"WorksheetRun(${marker.file}, ${expected})"
}
