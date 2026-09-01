package dotty.tools.repl.worksheet

import org.junit.Assert.*
import org.junit.Test

class WorksheetRenderingTest:
  private def summaryOf(value: String, screenWidth: Int): String =
    WorksheetRendering
      .render(WorksheetPosition(0, 0, 0, 9), List(RenderedBinder("x", "String", value)), "", screenWidth)
      .get
      .summary

  @Test def keepsASummaryThatFits(): Unit =
    assertEquals(": String = short", summaryOf("short", 120))

  @Test def collapsesAMultilineValueOntoOneLine(): Unit =
    assertEquals(": String = List( 1, 2 )", summaryOf("List(\n  1,\n  2\n)", 120))
