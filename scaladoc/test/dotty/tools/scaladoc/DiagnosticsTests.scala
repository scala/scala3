package dotty.tools.scaladoc

import org.junit.Test
import org.junit.Assert.*

class DiagnosticsTests:
  @Test
  def standardWarnings(): Unit =
    val ctx = testContext
    Scaladoc.run(Array("-no-indent", "doesnotexist.tasty"), ctx)
    assertEquals("", ctx.reportedDiagnostics.errorMsgs.mkString("\n"))
    assertEquals(
      "scaladoc will ignore following non-existent paths: doesnotexist.tasty\n"
       + "Destination is not provided, please provide '-d' parameter pointing to directory where docs should be created",
      ctx.reportedDiagnostics.warningMsgs.mkString("\n")
    )
    assertEquals("", ctx.reportedDiagnostics.infoMsgs.mkString("\n"))

  @Test
  def warnOnUnusedOptions(): Unit =
    val ctx = testContext
    Scaladoc.run(Array("-no-indent", "-warn-on-unused-options", "doesnotexist.tasty"), ctx)
    assertEquals("", ctx.reportedDiagnostics.errorMsgs.mkString("\n"))
    assertEquals(
      "Skipping unused scalacOptions: -no-indent\n"
      + "scaladoc will ignore following non-existent paths: doesnotexist.tasty\n"
      + "Destination is not provided, please provide '-d' parameter pointing to directory where docs should be created",
      ctx.reportedDiagnostics.warningMsgs.mkString("\n")
    )
    assertEquals("", ctx.reportedDiagnostics.infoMsgs.mkString("\n"))

  @Test
  def noArgsDisplaysHelp(): Unit =
    val ctx = testContext
    Scaladoc.run(Array(), ctx)
    assertEquals("", ctx.reportedDiagnostics.errorMsgs.mkString("\n"))
    assertEquals("", ctx.reportedDiagnostics.warningMsgs.mkString("\n"))
    assertTrue(ctx.reportedDiagnostics.infoMsgs.mkString("\n").contains("Usage: "))