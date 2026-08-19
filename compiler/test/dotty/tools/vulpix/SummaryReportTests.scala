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
      completedOverall = 42,
      failed = 1,
      elapsedSeconds = 125,
      activeTests = List(
        VulpixConsole.ActiveTest(3, "tests/neg/slow.scala", 70),
        VulpixConsole.ActiveTest(1, "tests/neg/other.scala", 45),
        VulpixConsole.ActiveTest(2, "tests/neg/third.scala", 20),
      ),
    )
    val expectedHeader =
      "[Vulpix] compileNeg: 12/20 done (60%); 3 active; 1 failed; 42 done overall; 2m05s"
    val expectedGitHub =
      """|::group::[Vulpix] compileNeg: 12/20 done (60%25); 3 active; 1 failed; 42 done overall; 2m05s
         |  worker 1: tests/neg/other.scala (45s)
         |  worker 2: tests/neg/third.scala (20s)
         |  worker 3: tests/neg/slow.scala (1m10s)
         |::endgroup::""".stripMargin

    assert(VulpixConsole.renderProgress(progress, useColors = false) == expectedHeader)
    assert(VulpixConsole.renderGitHubProgress(progress, useColors = false) == expectedGitHub)

    val colored = VulpixConsole.renderGitHubProgress(progress, useColors = true)
    assert(VulpixConsole.stripColors(colored) == expectedGitHub, colored)
    assert(colored.startsWith("::group::"), colored)

  @Test def progressPercentageAndEmptyWorkerRendering: Unit =
    def progress(completed: Int, total: Int) =
      VulpixConsole.Progress(Nil, completed, total, completed, 0, 0, Nil)

    assert(VulpixConsole.renderProgress(progress(0, 0), false).contains("done (100%)"))
    assert(VulpixConsole.renderProgress(progress(0, 3), false).contains("done (0%)"))
    assert(VulpixConsole.renderProgress(progress(1, 3), false).contains("done (33%)"))
    assert(VulpixConsole.renderProgress(progress(2, 3), false).contains("done (67%)"))
    assert(VulpixConsole.renderProgress(progress(758, 3886), false).contains("done (20%)"))
    assert(VulpixConsole.renderProgress(progress(9995, 10000), false).contains("done (99%)"))
    assert(VulpixConsole.renderProgress(progress(4, 3), false).contains("done (100%)"))
    assert(!VulpixConsole.renderGitHubProgress(progress(1, 3), false).contains("::group::"))

  @Test def progressCannotInjectWorkflowCommands: Unit =
    val progress = VulpixConsole.Progress(
      groupNames = List("compile%0A\n::endgroup::"),
      completed = 1,
      total = 2,
      completedOverall = 1,
      failed = 0,
      elapsedSeconds = 1,
      activeTests = List(VulpixConsole.ActiveTest(1, "\n::error::injected\u001b", 1)),
    )
    val rendered = VulpixConsole.renderGitHubProgress(progress, useColors = false)
    val lines = rendered.linesIterator.toList

    assert(lines.count(_.startsWith("::group::")) == 1, rendered)
    assert(lines.count(_ == "::endgroup::") == 1, rendered)
    assert(!lines.exists(_.startsWith("::error::")), rendered)
    assert(lines.head.contains("%250A"), rendered)

  @Test def cumulativeProgressExcludesNestedRuns: Unit =
    val report = SummaryReport()
    report.reportResults(passed = 7, failed = List(FailedTestInfo("bad.scala", "")), skipped = 2)
    assert(report.completedSources == 10)
    report.reportResults(passed = 0, failed = Nil, skipped = 3)
    assert(report.completedSources == 13)

    val nested = NoResultSummaryReport(report)
    nested.reportResults(passed = 5, failed = Nil, skipped = 0)
    assert(report.completedSources == 13)

  @Test def githubActionsColorDetection: Unit =
    val github = Map("GITHUB_ACTIONS" -> "true")
    assert(VulpixConsole.githubActionsEnabled(github, isCI = true))
    assert(VulpixConsole.githubActionsEnabled(github + ("NO_COLOR" -> ""), isCI = true))
    assert(!VulpixConsole.githubActionsEnabled(github, isCI = false))
    assert(VulpixConsole.colorsEnabled(github, isCI = true))
    assert(!VulpixConsole.colorsEnabled(github, isCI = false))
    assert(!VulpixConsole.colorsEnabled(Map.empty, isCI = true))
    assert(!VulpixConsole.colorsEnabled(github + ("NO_COLOR" -> ""), isCI = true))
