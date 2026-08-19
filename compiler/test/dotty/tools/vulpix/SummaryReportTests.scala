package dotty.tools.vulpix

import java.nio.charset.StandardCharsets.UTF_8
import org.junit.Test

class SummaryReportTests:
  private def millis(value: Long): Long = value * 1_000_000L

  @Test def summaryRendering: Unit =
    val report = SummaryReport()
    report.reportResults(2, List(FailedTestInfo("tests/z.scala", " failed"), FailedTestInfo("tests/a.scala", " failed")), 1)
    report.reportResults(1, Nil, 0)
    report.addReproduceInstruction("REPRODUCE_SENTINEL")
    val expected =
      """|== Vulpix Test Report: 3 tests passed, 2 failed, 1 skipped, 6 total ==
         |Failed tests:
         |    tests/a.scala failed
         |    tests/z.scala failed""".stripMargin

    val plain = VulpixConsole.renderSummary(report.summary, useColors = false)
    assert(plain == expected, plain)
    assert(report.reproductionText == "REPRODUCE_SENTINEL")
    assert(!plain.contains("REPRODUCE_SENTINEL"))
    val colored = VulpixConsole.renderSummary(report.summary, useColors = true)
    assert(VulpixConsole.stripColors(colored) == expected, colored)
    assert(colored.contains("\u001b["), colored)

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
    val output = new java.io.ByteArrayOutputStream
    Console.withOut(output) {
      val live = SummaryReport(pulse = true)
      List("patmat", "patmat", "runAll").foreach(group => live.beginTestGroups(Set(group)))
    }
    assert(VulpixConsole.stripColors(output.toString(UTF_8)).linesIterator.toList ==
      List("[Vulpix] Starting patmat", "[Vulpix] Starting runAll"))

    val github = Map("VULPIX_CI" -> "true", "GITHUB_ACTIONS" -> "true")
    assert(VulpixConsole.ciEnabled(github, isCI = true))
    assert(!VulpixConsole.ciEnabled(github, isCI = false))
    assert(VulpixConsole.colorsEnabled(github, isCI = true))
    assert(!VulpixConsole.colorsEnabled(github + ("NO_COLOR" -> ""), isCI = true))
    val debug = github + ("RUNNER_DEBUG" -> "1")
    assert(VulpixConsole.pulseEnabled(debug, isCI = true))
    assert(!VulpixConsole.pulseEnabled(github, isCI = true))

  @Test def slowestRendering: Unit =
    val timings = List(
      VulpixConsole.TestTiming("compileNeg", "slow.scala", millis(125_432)),
      VulpixConsole.TestTiming("compileNeg", "a.scala", millis(12_842)),
      VulpixConsole.TestTiming("compileNeg", "b.scala", millis(12_842)),
      VulpixConsole.TestTiming("compileNeg", "mid.scala", millis(3_000)),
      VulpixConsole.TestTiming("compileNeg", "quick.scala", millis(842)),
      VulpixConsole.TestTiming("compileNeg", "quick.scala", millis(900)),
      VulpixConsole.TestTiming("compileNeg", "excluded.scala", millis(1)),
      VulpixConsole.TestTiming("compilePos", "other.scala", millis(60_001)),
    )
    val overall =
      """|[Vulpix] Top 5 slowest overall:
         |  1. [compileNeg] slow.scala (2m05.432s)
         |  2. [compilePos] other.scala (1m00.001s)
         |  3. [compileNeg] a.scala (12.842s)
         |  4. [compileNeg] b.scala (12.842s)
         |  5. [compileNeg] mid.scala (3.000s)""".stripMargin
    val byGroup = VulpixConsole.renderSlowestByGroup(timings, false)
    assert(byGroup.linesIterator.filter(_.startsWith("[Vulpix]")).toList ==
      List("[Vulpix] Top 5 slowest in compileNeg:", "[Vulpix] Top 5 slowest in compilePos:"))
    assert(byGroup.contains("5. quick.scala (900ms)") && !byGroup.contains("excluded.scala"), byGroup)
    assert(VulpixConsole.renderSlowestOverall(timings, false) == overall)
    assert(VulpixConsole.renderSlowestOverall(Nil, false).isEmpty)

  @Test def timingAccumulationAndDrain: Unit =
    val child = TestGroup("topLevel").child("internal/multi-file")
    assert(child.reportingName == "topLevel" && child.name == "internal/multi-file")
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
