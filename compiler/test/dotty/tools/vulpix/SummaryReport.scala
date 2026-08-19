package dotty
package tools
package vulpix

import dotc.reporting.TestReporter
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ConcurrentLinkedDeque
import scala.collection.mutable.ArrayBuffer

/** Collects Vulpix results, writes summaries to stdout, and records failure diagnostics.
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

  /** Record timings for a completed Vulpix batch. */
  private[vulpix] def reportTestTimings(timings: Iterable[VulpixConsole.TestTiming]): Unit = ()

  /** Finish timing groups that are not part of the next Vulpix batch. */
  private[vulpix] def beginTestGroups(groups: Set[String]): Unit = ()

  /** Display the accumulated timings for each group in the completed suite. */
  private[vulpix] def flushTestTimings(): Unit = ()

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

/** A summary report that writes concise status to stdout and failure details to `./testlogs/`. */
final class SummaryReport(pulse: Boolean = VulpixConsole.pulseEnabled) extends SummaryReporting {
  import scala.jdk.CollectionConverters.*

  private val failedTests = new ConcurrentLinkedDeque[FailedTestInfo]
  private val skippedTests = new ConcurrentLinkedDeque[FailedTestInfo]
  private val reproduceInstructions = new ConcurrentLinkedDeque[String]
  private val testTimings = new ConcurrentLinkedDeque[VulpixConsole.TestTiming]
  private val pendingTestTimings = ArrayBuffer.empty[VulpixConsole.TestTiming]

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

  private def summary: VulpixConsole.Summary =
    VulpixConsole.Summary(passed.get, failedTestsSnapshot, skipped.get)

  private[vulpix] def summaryText: String =
    VulpixConsole.renderSummary(summary, useColors = false)

  private[vulpix] def consoleSummaryText(useColors: Boolean): String =
    VulpixConsole.renderSummary(summary, useColors)

  private[vulpix] def reproductionText: String =
    reproduceInstructions.asScala.toList.distinct.sorted.mkString

  private[vulpix] def overallTimingsText: String =
    VulpixConsole.renderSlowestOverall(testTimings.asScala, useColors = false)

  /** Echo the concise summary to stdout; local runs also log reproduction instructions. */
  override def echoSummary(): Unit = {
    flushTestTimings()
    val failures = failedTestsSnapshot
    val hasResults = passed.get + failures.size + skipped.get > 0
    TestReporter.writeFailedTests(failures.map(_.title).distinct)

    if hasResults then println(consoleSummaryText(VulpixConsole.colorsEnabled))

    val overallTimings =
      if progressEnabled then VulpixConsole.renderSlowestOverall(testTimings.asScala, VulpixConsole.colorsEnabled)
      else ""
    if overallTimings.nonEmpty then println(overallTimings)

    if !Properties.isRunByCI then {
      skippedTests.asScala.map(x => s"    ${x.title} skipped").toList.distinct.sorted.foreach(println)
      if failures.nonEmpty then println {
        s"""|--------------------------------------------------------------------------------
            |Note - reproduction instructions have been dumped to log file:
            |    ${TestReporter.logPath}
            |--------------------------------------------------------------------------------""".stripMargin
      }
    }

    val reproduction = reproductionText
    if reproduction.nonEmpty && !Properties.isRunByCI then echoToLog(List(reproduction))
  }

  override private[vulpix] def progressEnabled: Boolean = pulse

  override private[vulpix] def echoProgress(progress: VulpixConsole.Progress): Unit =
    if progressEnabled then println(VulpixConsole.renderProgress(progress, VulpixConsole.colorsEnabled))

  override private[vulpix] def reportTestTimings(timings: Iterable[VulpixConsole.TestTiming]): Unit = {
    val snapshot = timings.toList
    synchronized {
      snapshot.foreach(testTimings.add)
      pendingTestTimings ++= snapshot
    }
  }

  override private[vulpix] def beginTestGroups(groups: Set[String]): Unit =
    if progressEnabled then emitTestTimings(drainTestTimingsExcept(groups))

  override private[vulpix] def flushTestTimings(): Unit =
    if progressEnabled then emitTestTimings(drainTestTimingsExcept(Set.empty))

  private[vulpix] def drainTestTimingsExcept(groups: Set[String]): List[VulpixConsole.TestTiming] = synchronized {
    val (retained, completed) = pendingTestTimings.partition(timing => groups.contains(timing.group))
    pendingTestTimings.clear()
    pendingTestTimings ++= retained
    completed.toList
  }

  private def emitTestTimings(timings: Iterable[VulpixConsole.TestTiming]): Unit =
    if timings.nonEmpty then {
      val rendered = VulpixConsole.renderSlowestByGroup(timings, VulpixConsole.colorsEnabled)
      if rendered.nonEmpty then println(rendered)
    }

  override def echoToLog(it: Iterable[String]): Unit = synchronized {
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
