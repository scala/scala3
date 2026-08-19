package dotty
package tools
package vulpix

import dotc.reporting.TestReporter
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ConcurrentLinkedDeque

/** Collects Vulpix results and writes summaries to stdout and the test log.
 *
 *  Local runs use one report per JUnit suite and emit it from `@AfterClass`.
 *  CI runs share one report across the test process and emit it from a shutdown
 *  hook after all suites have completed.
 */
trait SummaryReporting {
  /** Report the result of running a collection of test sources. */
  def reportResults(passed: Int, failed: Iterable[FailedTestInfo], skipped: Int): Unit

  /** Add a skipped test. */
  def addSkippedTest(msg: FailedTestInfo): Unit

  /** Add instructions to reproduce the error */
  def addReproduceInstruction(instr: String): Unit

  /** Echo the summary report to the appropriate locations */
  def echoSummary(): Unit

  /** Echoes contents of `it` to file *immediately* then flushes */
  def echoToLog(it: Iterable[String]): Unit

}

/** A summary report that doesn't do anything */
final class NoSummaryReport extends SummaryReporting {
  override def reportResults(passed: Int, failed: Iterable[FailedTestInfo], skipped: Int): Unit = ()
  override def addSkippedTest(msg: FailedTestInfo): Unit = ()
  override def addReproduceInstruction(instr: String): Unit = ()
  override def echoSummary(): Unit = ()
  override def echoToLog(it: Iterable[String]): Unit = ()
}

private[vulpix] final class NoResultSummaryReport(delegate: SummaryReporting) extends SummaryReporting {
  override def reportResults(passed: Int, failed: Iterable[FailedTestInfo], skipped: Int): Unit = ()
  override def addSkippedTest(msg: FailedTestInfo): Unit = delegate.addSkippedTest(msg)
  override def addReproduceInstruction(instr: String): Unit = delegate.addReproduceInstruction(instr)
  override def echoSummary(): Unit = ()
  override def echoToLog(it: Iterable[String]): Unit = delegate.echoToLog(it)
}

/** A summary report that logs to both stdout and the `TestReporter.logWriter`
 *  which outputs to a log file in `./testlogs/`
 */
final class SummaryReport extends SummaryReporting {
  import scala.jdk.CollectionConverters.*

  private val failedTests = new ConcurrentLinkedDeque[FailedTestInfo]
  private val skippedTests = new ConcurrentLinkedDeque[FailedTestInfo]
  private val reproduceInstructions = new ConcurrentLinkedDeque[String]

  private val passed = AtomicInteger()
  private val skipped = AtomicInteger()

  override def reportResults(passed: Int, failed: Iterable[FailedTestInfo], skipped: Int): Unit = {
    require(passed >= 0 && skipped >= 0)
    this.passed.addAndGet(passed)
    this.skipped.addAndGet(skipped)
    failed.foreach(failedTests.add)
  }

  override def addSkippedTest(msg: FailedTestInfo): Unit =
    skippedTests.add(msg)

  override def addReproduceInstruction(instr: String): Unit =
    reproduceInstructions.add(instr)

  private def failedTestsSnapshot: List[FailedTestInfo] =
    failedTests.asScala.toList.sortBy(info => (info.title, info.extra))

  private[vulpix] def summaryText: String = {
    val failures = failedTestsSnapshot
    val passedCount = passed.get
    val skippedCount = skipped.get
    val total = passedCount + failures.size + skippedCount
    val testLabel = if passedCount == 1 then "test" else "tests"
    val skippedText = if skippedCount == 0 then "" else s", $skippedCount skipped"
    val failedLines =
      if failures.isEmpty then ""
      else failures.map(info => s"    ${info.title}${info.extra}").mkString("Failed tests:\n", "\n", "\n")

    s"""|================================================================================
        |Vulpix Test Report
        |================================================================================
        |
        |$passedCount $testLabel passed, ${failures.size} failed$skippedText, $total total
        |$failedLines""".stripMargin
  }

  private[vulpix] def detailedText: String = {
    val instructions = reproduceInstructions.asScala.toList.sorted
    if instructions.isEmpty then summaryText
    else instructions.mkString(summaryText + "\n", "", "")
  }

  /** Echo the concise summary to stdout and the full report to file. */
  override def echoSummary(): Unit = {
    val failures = failedTestsSnapshot
    val hasResults = passed.get + failures.size + skipped.get > 0
    TestReporter.writeFailedTests(failures.map(_.title).distinct)

    if hasResults then println(summaryText)

    if !Properties.isRunByCI then {
      skippedTests.asScala.map(x => s"    ${x.title} skipped").toList.distinct.sorted.foreach(println)
      if failures.nonEmpty then println {
        s"""|--------------------------------------------------------------------------------
            |Note - reproduction instructions have been dumped to log file:
            |    ${TestReporter.logPath}
            |--------------------------------------------------------------------------------""".stripMargin
      }
    }

    if hasResults || !reproduceInstructions.isEmpty then TestReporter.logPrintln(detailedText)
  }

  private def removeColors(msg: String): String =
    msg.replaceAll("\u001b\\[.*?m", "")

  override def echoToLog(it: Iterable[String]): Unit = {
    it.foreach(msg => TestReporter.logPrint(removeColors(msg)))
    TestReporter.logFlush()
  }
}

object SummaryReport {
  private lazy val ciReport = {
    val report = SummaryReport()
    Runtime.getRuntime.addShutdownHook(new Thread(() => report.echoSummary(), "vulpix-summary"))
    report
  }

  /** CI uses one process-wide report, emitted after all JUnit suites have run. */
  def default: SummaryReport =
    if Properties.isRunByCI then ciReport else SummaryReport()
}

case class FailedTestInfo(title: String, extra: String)
