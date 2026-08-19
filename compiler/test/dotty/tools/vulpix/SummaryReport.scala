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

  /** Whether this reporter wants periodic CI progress updates. */
  private[vulpix] def progressEnabled: Boolean = false

  /** Echo a periodic CI progress update. */
  private[vulpix] def echoProgress(progress: VulpixConsole.Progress): Unit = ()

  /** Number of logical test sources completed by earlier Vulpix batches. */
  private[vulpix] def completedSources: Int = 0

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
  private val completed = AtomicInteger()

  override def reportResults(passed: Int, failed: Iterable[FailedTestInfo], skipped: Int): Unit = {
    require(passed >= 0 && skipped >= 0)
    var failedCount = 0
    this.passed.addAndGet(passed)
    this.skipped.addAndGet(skipped)
    failed.foreach { failure =>
      failedTests.add(failure)
      failedCount += 1
    }
    completed.addAndGet(passed + failedCount + skipped)
  }

  override def addSkippedTest(msg: FailedTestInfo): Unit =
    skippedTests.add(msg)

  override def addReproduceInstruction(instr: String): Unit =
    reproduceInstructions.add(instr)

  private def failedTestsSnapshot: List[FailedTestInfo] =
    failedTests.asScala.toList.sortBy(info => (info.title, info.extra))

  private def summary: VulpixConsole.Summary =
    VulpixConsole.Summary(passed.get, failedTestsSnapshot, skipped.get)

  private[vulpix] def summaryText: String =
    VulpixConsole.renderSummary(summary, useColors = false)

  private[vulpix] def consoleSummaryText(useColors: Boolean): String =
    VulpixConsole.renderSummary(summary, useColors)

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

    if hasResults then println(consoleSummaryText(VulpixConsole.colorsEnabled))

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

  override private[vulpix] def progressEnabled: Boolean = Properties.isRunByCI

  override private[vulpix] def echoProgress(progress: VulpixConsole.Progress): Unit =
    if progressEnabled then
      val rendered =
        if VulpixConsole.githubActionsEnabled then
          VulpixConsole.renderGitHubProgress(progress, VulpixConsole.colorsEnabled)
        else VulpixConsole.renderProgress(progress, useColors = false)
      println(rendered)

  override private[vulpix] def completedSources: Int = completed.get

  override def echoToLog(it: Iterable[String]): Unit = {
    it.foreach(msg => TestReporter.logPrint(VulpixConsole.stripColors(msg)))
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
