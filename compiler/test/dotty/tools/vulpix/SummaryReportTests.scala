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
