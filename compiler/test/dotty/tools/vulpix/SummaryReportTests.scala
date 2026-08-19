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
        VulpixConsole.ActiveTest("tests/neg/third.scala", 20),
        VulpixConsole.ActiveTest("tests/neg/slow.scala", 70),
        VulpixConsole.ActiveTest("tests/neg/other.scala", 45),
      ),
    )
    val expected =
      "[Vulpix] compileNeg | 12/20 complete | 3 running | 1 failed in group | 2m05s elapsed" +
        " | longest: tests/neg/slow.scala (1m10s), tests/neg/other.scala (45s), +1"

    assert(VulpixConsole.renderProgress(progress, useColors = false) == expected)
    val colored = VulpixConsole.renderProgress(progress, useColors = true)
    assert(VulpixConsole.stripColors(colored) == expected, colored)

  @Test def progressRenderingIsSingleLineWithoutActiveTests: Unit =
    val progress = VulpixConsole.Progress(
      groupNames = List("compile\u001b\n::error::injected"),
      completed = 20,
      total = 20,
      failed = 0,
      elapsedSeconds = 1,
      activeTests = Nil,
    )
    val rendered = VulpixConsole.renderProgress(progress, useColors = false)

    assert(rendered.linesIterator.size == 1, rendered)
    assert(!rendered.contains('\u001b'), rendered)
    assert(!rendered.contains(" | longest:"), rendered)
    assert(rendered.contains("compile??::error::injected"), rendered)

    val withActive = VulpixConsole.renderProgress(
      progress.copy(activeTests = List(VulpixConsole.ActiveTest("tests/pos/ok.scala\u001b\n::error::injected", 1))),
      useColors = false,
    )
    assert(withActive.linesIterator.size == 1, withActive)
    assert(withActive.contains("tests/pos/ok.scala??::error::injected"), withActive)

  @Test def githubActionsColorDetection: Unit =
    val github = Map("GITHUB_ACTIONS" -> "true")
    assert(VulpixConsole.colorsEnabled(github, isCI = true))
    assert(!VulpixConsole.colorsEnabled(github, isCI = false))
    assert(!VulpixConsole.colorsEnabled(Map.empty, isCI = true))
    assert(!VulpixConsole.colorsEnabled(github + ("NO_COLOR" -> ""), isCI = true))

  @Test def pulseDetection: Unit =
    val enabled = Map("VULPIX_CI_PULSE" -> "true")
    assert(VulpixConsole.pulseEnabled(enabled, isCI = true))
    assert(!VulpixConsole.pulseEnabled(enabled, isCI = false))
    assert(!VulpixConsole.pulseEnabled(Map.empty, isCI = true))
    assert(!VulpixConsole.pulseEnabled(Map("VULPIX_CI_PULSE" -> "false"), isCI = true))

  @Test def slowestTestRendering: Unit =
    def millis(value: Long): Long = value * 1_000_000L
    val timings = List(
      VulpixConsole.TestTiming("compileNeg", "tests/neg/slow.scala", millis(125_432)),
      VulpixConsole.TestTiming("compileNeg", "tests/neg/a.scala", millis(12_842)),
      VulpixConsole.TestTiming("compileNeg", "tests/neg/b.scala", millis(12_842)),
      VulpixConsole.TestTiming("compileNeg", "tests/neg/mid.scala", millis(3_000)),
      VulpixConsole.TestTiming("compileNeg", "tests/neg/quick.scala", millis(842)),
      VulpixConsole.TestTiming("compileNeg", "tests/neg/excluded.scala", millis(1)),
      VulpixConsole.TestTiming("compilePos", "tests/pos/slow\u001b\n::error::injected.scala", millis(3_600_001)),
    )
    val expectedByGroup =
      """|[Vulpix] Top 5 slowest in compileNeg:
         |  1. tests/neg/slow.scala (2m05.432s)
         |  2. tests/neg/a.scala (12.842s)
         |  3. tests/neg/b.scala (12.842s)
         |  4. tests/neg/mid.scala (3.000s)
         |  5. tests/neg/quick.scala (842ms)
         |[Vulpix] Top 5 slowest in compilePos:
         |  1. tests/pos/slow??::error::injected.scala (1h00m00.001s)""".stripMargin
    val expectedOverall =
      """|[Vulpix] Top 5 slowest overall:
         |  1. [compilePos] tests/pos/slow??::error::injected.scala (1h00m00.001s)
         |  2. [compileNeg] tests/neg/slow.scala (2m05.432s)
         |  3. [compileNeg] tests/neg/a.scala (12.842s)
         |  4. [compileNeg] tests/neg/b.scala (12.842s)
         |  5. [compileNeg] tests/neg/mid.scala (3.000s)""".stripMargin

    assert(VulpixConsole.renderSlowestByGroup(timings, useColors = false) == expectedByGroup)
    assert(VulpixConsole.renderSlowestOverall(timings, useColors = false) == expectedOverall)
    assert(VulpixConsole.renderSlowestByGroup(Nil, useColors = false).isEmpty)
    assert(VulpixConsole.renderSlowestOverall(Nil, useColors = false).isEmpty)

    val colored = VulpixConsole.renderSlowestOverall(timings, useColors = true)
    assert(VulpixConsole.stripColors(colored) == expectedOverall, colored)

  @Test def timingAccumulationExcludesNestedRuns: Unit =
    val report = SummaryReport(pulse = false)
    val first = VulpixConsole.TestTiming("compilePos", "tests/pos/first.scala", 2_000_000L)
    val second = VulpixConsole.TestTiming("compileNeg", "tests/neg/second.scala", 1_000_000L)
    report.reportTestTimings(List(first))
    report.reportTestTimings(List(second))
    val expected = VulpixConsole.renderSlowestOverall(List(first, second), useColors = false)
    assert(report.overallTimingsText == expected)

    NoResultSummaryReport(report).reportTestTimings(
      List(VulpixConsole.TestTiming("nested", "should-not-appear.scala", Long.MaxValue))
    )
    assert(report.overallTimingsText == expected)

  @Test def groupTimingsCoalesceUntilNextGroup: Unit =
    val report = SummaryReport(pulse = false)
    val timings = List(
      VulpixConsole.TestTiming("compilePos", "tests/pos/first.scala", 3_000_000L),
      VulpixConsole.TestTiming("compilePos", "tests/pos/second.scala", 2_000_000L),
      VulpixConsole.TestTiming("compileNeg", "tests/neg/third.scala", 1_000_000L),
    )
    report.reportTestTimings(timings.take(1))
    assert(report.drainTestTimingsExcept(Set("compilePos")).isEmpty)

    report.reportTestTimings(timings.slice(1, 2))
    assert(report.drainTestTimingsExcept(Set("compileNeg")) == timings.take(2))

    report.reportTestTimings(timings.drop(2))
    assert(report.drainTestTimingsExcept(Set.empty) == timings.drop(2))
    assert(report.drainTestTimingsExcept(Set.empty).isEmpty)
    assert(report.overallTimingsText == VulpixConsole.renderSlowestOverall(timings, useColors = false))

  @Test def startMarkerDetection: Unit =
    val environment = Map("VULPIX_CI_START_MARKER" -> "safe-marker")
    assert(VulpixConsole.startMarker(environment, isCI = true).contains("safe-marker"))
    assert(VulpixConsole.startMarker(environment, isCI = false).isEmpty)
    assert(VulpixConsole.startMarker(Map.empty, isCI = true).isEmpty)
    assert(VulpixConsole.startMarker(environment.updated("VULPIX_CI_START_MARKER", "bad\nmarker"), isCI = true).isEmpty)
