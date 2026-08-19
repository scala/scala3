package dotty.tools.vulpix

import org.junit.Test

class SummaryReportTests:
  private def millis(value: Long): Long = value * 1_000_000L

  @Test def summaryRendering: Unit =
    val report = SummaryReport()
    report.reportResults(2, List(FailedTestInfo("tests/z.scala", " failed"), FailedTestInfo("tests/a.scala", " failed")), 1)
    report.reportResults(1, List(FailedTestInfo("tests/m.scala", " failed, test timed out")), 0)
    report.addReproduceInstruction("REPRODUCE_SENTINEL")
    val expected =
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

    assert(report.summaryText == expected, report.summaryText)
    assert(report.reproductionText == "REPRODUCE_SENTINEL")
    assert(!report.summaryText.contains("REPRODUCE_SENTINEL"))
    val colored = report.summaryText(useColors = true)
    assert(VulpixConsole.stripColors(colored) == expected, colored)
    List(31, 32, 33).foreach(code => assert(colored.contains(s"\u001b[${code}m"), colored))
    assert(!report.reproductionText.contains("\u001b["))

  @Test def progressRenderingAndEnvironmentGates: Unit =
    val progress = VulpixConsole.Progress(
      List("compileNeg\u001b\n::error::group"), 12, 20, 1, 125,
      List(
        VulpixConsole.ActiveTest("tests/third.scala", 20),
        VulpixConsole.ActiveTest("tests/slow\u001b\n::error::test.scala", 70),
        VulpixConsole.ActiveTest("tests/other.scala", 45),
      ),
    )
    val expected =
      "[Vulpix] compileNeg??::error::group | 12/20 complete | 3 running | 1 failed in group | 2m05s elapsed" +
        " | longest: tests/slow??::error::test.scala (1m10s), tests/other.scala (45s), +1"
    val rendered = VulpixConsole.renderProgress(progress, useColors = false)
    assert(rendered == expected, rendered)
    assert(rendered.linesIterator.size == 1 && !rendered.contains('\u001b'), rendered)
    assert(VulpixConsole.stripColors(VulpixConsole.renderProgress(progress, useColors = true)) == expected)
    assert(VulpixConsole.renderGroupStarts(Set("patmat", "parallelBackend"), false) ==
      "[Vulpix] Starting parallelBackend\n[Vulpix] Starting patmat")

    val output = java.io.ByteArrayOutputStream()
    Console.withOut(output) {
      val live = SummaryReport(pulse = true)
      live.beginTestGroups(Set("patmat"))
      live.beginTestGroups(Set("patmat"))
      live.beginTestGroups(Set("runAll"))
    }
    assert(output.toString("UTF-8").linesIterator.toList ==
      List("[Vulpix] Starting patmat", "[Vulpix] Starting runAll"))

    val github = Map("GITHUB_ACTIONS" -> "true")
    assert(VulpixConsole.colorsEnabled(github, isCI = true))
    assert(!VulpixConsole.colorsEnabled(github, isCI = false))
    assert(!VulpixConsole.colorsEnabled(Map.empty, isCI = true))
    assert(!VulpixConsole.colorsEnabled(github + ("NO_COLOR" -> ""), isCI = true))
    val debug = Map("RUNNER_DEBUG" -> "1")
    assert(VulpixConsole.pulseEnabled(debug, isCI = true))
    assert(!VulpixConsole.pulseEnabled(debug, isCI = false))
    assert(!VulpixConsole.pulseEnabled(Map.empty, isCI = true))
    assert(!VulpixConsole.pulseEnabled(Map("RUNNER_DEBUG" -> "true"), isCI = true))

  @Test def slowestRendering: Unit =
    val timings = List(
      VulpixConsole.TestTiming("compileNeg", "slow.scala", millis(125_432)),
      VulpixConsole.TestTiming("compileNeg", "a.scala", millis(12_842)),
      VulpixConsole.TestTiming("compileNeg", "b.scala", millis(12_842)),
      VulpixConsole.TestTiming("compileNeg", "mid.scala", millis(3_000)),
      VulpixConsole.TestTiming("compileNeg", "quick.scala", millis(842)),
      VulpixConsole.TestTiming("compileNeg", "quick.scala", millis(900)),
      VulpixConsole.TestTiming("compileNeg", "excluded.scala", millis(1)),
      VulpixConsole.TestTiming("compilePos", "hour.scala", millis(3_600_001)),
    )
    val byGroup =
      """|[Vulpix] Top 5 slowest in compileNeg:
         |  1. slow.scala (2m05.432s)
         |  2. a.scala (12.842s)
         |  3. b.scala (12.842s)
         |  4. mid.scala (3.000s)
         |  5. quick.scala (900ms)
         |[Vulpix] Top 5 slowest in compilePos:
         |  1. hour.scala (1h00m00.001s)""".stripMargin
    val overall =
      """|[Vulpix] Top 5 slowest overall:
         |  1. [compilePos] hour.scala (1h00m00.001s)
         |  2. [compileNeg] slow.scala (2m05.432s)
         |  3. [compileNeg] a.scala (12.842s)
         |  4. [compileNeg] b.scala (12.842s)
         |  5. [compileNeg] mid.scala (3.000s)""".stripMargin
    assert(VulpixConsole.renderSlowestByGroup(timings, false) == byGroup)
    assert(VulpixConsole.renderSlowestOverall(timings, false) == overall)
    assert(VulpixConsole.renderSlowestOverall(Nil, false).isEmpty)

  @Test def timingAccumulationAndDrain: Unit =
    val report = SummaryReport(pulse = false)
    val first = VulpixConsole.TestTiming("compilePos", "first.scala", 3_000_000L)
    val second = VulpixConsole.TestTiming("compilePos", "second.scala", 2_000_000L)
    val third = VulpixConsole.TestTiming("compileNeg", "third.scala", 1_000_000L)
    report.reportTestTimings(List(first))
    assert(report.drainTestTimingsExcept(Set("compilePos")).isEmpty)
    report.reportTestTimings(List(second, third))
    NoResultSummaryReport(report).reportTestTimings(List(VulpixConsole.TestTiming("nested", "hidden.scala", Long.MaxValue)))
    assert(report.drainTestTimingsExcept(Set("compileNeg")) == List(first, second))
    assert(report.drainTestTimingsExcept(Set.empty) == List(third))
    assert(!report.overallTimingsText.contains("hidden.scala"), report.overallTimingsText)
