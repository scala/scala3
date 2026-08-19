package dotty.tools.vulpix

import org.junit.Test

class SummaryReportTests:
  @Test def conciseAndDetailedReports: Unit =
    val report = SummaryReport()
    report.reportResults(
      passed = 2,
      failed = List(
        FailedTestInfo("tests/z.scala", " failed"),
        FailedTestInfo("tests/a.scala", " failed"),
      ),
      skipped = 1,
    )
    report.reportResults(
      passed = 1,
      failed = List(FailedTestInfo("tests/m.scala", " failed, test timed out")),
      skipped = 0,
    )
    report.addReproduceInstruction("REPRODUCE_SENTINEL")

    val expectedSummary =
      """|================================================================================
         |Vulpix Test Report
         |================================================================================
         |
         |3 tests passed, 3 failed, 1 skipped, 7 total
         |Failed tests:
         |    tests/a.scala failed
         |    tests/m.scala failed, test timed out
         |    tests/z.scala failed
         |""".stripMargin

    assert(report.summaryText == expectedSummary, report.summaryText)
    assert(!report.summaryText.contains("REPRODUCE_SENTINEL"))
    assert(report.detailedText == expectedSummary + "\nREPRODUCE_SENTINEL")

    val coloredSummary = report.consoleSummaryText(useColors = true)
    assert(VulpixConsole.stripColors(coloredSummary) == expectedSummary, coloredSummary)
    assert(coloredSummary.contains("\u001b[32m"), coloredSummary)
    assert(coloredSummary.contains("\u001b[31m"), coloredSummary)
    assert(coloredSummary.contains("\u001b[33m"), coloredSummary)
    assert(!report.detailedText.contains("\u001b["), report.detailedText)

  @Test def progressRendering: Unit =
    val progress = VulpixConsole.Progress(
      groupNames = List("compileNeg"),
      completed = 12,
      total = 20,
      failed = 1,
      elapsedSeconds = 125,
      activeTests = List(
        VulpixConsole.ActiveTest("tests/neg/slow.scala", 70),
        VulpixConsole.ActiveTest("tests/neg/other.scala", 45),
        VulpixConsole.ActiveTest("tests/neg/third.scala", 20),
      ),
    )
    val expected =
      "[Vulpix] compileNeg | 12/20 complete | 3 running | 1 failed in group | 2m05s elapsed" +
        " | longest: tests/neg/slow.scala (1m10s), tests/neg/other.scala (45s), +1"

    assert(VulpixConsole.renderProgress(progress, useColors = false) == expected)
    val colored = VulpixConsole.renderProgress(progress, useColors = true)
    assert(VulpixConsole.stripColors(colored) == expected, colored)

  @Test def githubActionsColorDetection: Unit =
    val github = Map("GITHUB_ACTIONS" -> "true")
    assert(VulpixConsole.colorsEnabled(github, isCI = true))
    assert(!VulpixConsole.colorsEnabled(github, isCI = false))
    assert(!VulpixConsole.colorsEnabled(Map.empty, isCI = true))
    assert(!VulpixConsole.colorsEnabled(github + ("NO_COLOR" -> ""), isCI = true))
