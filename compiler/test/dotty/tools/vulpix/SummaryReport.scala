package dotty
package tools
package vulpix

import dotc.reporting.TestReporter
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

  /** Add instructions to reproduce the error */
  def addReproduceInstruction(instr: String): Unit

  /** Echo the summary report to the appropriate locations */
  def echoSummary(): Unit

  /** Echoes contents of `it` to file *immediately* then flushes */
  def echoToLog(it: Iterable[String]): Unit

  /** Whether this reporter wants periodic CI progress updates. */
  private[vulpix] def progressEnabled: Boolean = false

  /** Whether this reporter wants top-level CI group announcements. */
  private[vulpix] def groupAnnouncementsEnabled: Boolean = false

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
  override def addReproduceInstruction(instr: String): Unit = ()
  override def echoSummary(): Unit = ()
  override def echoToLog(it: Iterable[String]): Unit = ()
}

private[vulpix] final class NoResultSummaryReport(delegate: SummaryReporting) extends SummaryReporting {
  override def reportResults(passed: Int, failed: Iterable[FailedTestInfo], skipped: Int): Unit = ()
  override def addReproduceInstruction(instr: String): Unit = delegate.addReproduceInstruction(instr)
  override def echoSummary(): Unit = ()
  override def echoToLog(it: Iterable[String]): Unit = delegate.echoToLog(it)
}

/** A summary report that writes concise status to stdout and failure details to `./testlogs/`. */
final class SummaryReport(
  pulse: Boolean = VulpixConsole.pulseEnabled,
  announceGroups: Boolean = Properties.isRunByCI,
) extends SummaryReporting {
  private val failedTests = ArrayBuffer.empty[FailedTestInfo]
  private val reproduceInstructions = ArrayBuffer.empty[String]
  private val testTimings = ArrayBuffer.empty[VulpixConsole.TestTiming]
  private val pendingTestTimings = ArrayBuffer.empty[VulpixConsole.TestTiming]
  private var currentTestGroups = Set.empty[String]

  private var passed = 0
  private var skipped = 0

  override def reportResults(passed: Int, failed: Iterable[FailedTestInfo], skipped: Int): Unit = synchronized {
    require(passed >= 0 && skipped >= 0)
    this.passed += passed
    this.skipped += skipped
    failedTests ++= failed
  }

  override def addReproduceInstruction(instr: String): Unit = synchronized {
    reproduceInstructions += instr
  }

  private def summary: VulpixConsole.Summary =
    synchronized(VulpixConsole.Summary(passed, failedTests.toList.sortBy(info => (info.title, info.extra)), skipped))

  private[vulpix] def summaryText: String =
    VulpixConsole.renderSummary(summary, useColors = false)

  private[vulpix] def summaryText(useColors: Boolean): String =
    VulpixConsole.renderSummary(summary, useColors)

  private[vulpix] def reproductionText: String =
    synchronized(reproduceInstructions.toList).distinct.sorted.mkString

  private[vulpix] def overallTimingsText: String =
    VulpixConsole.renderSlowestOverall(synchronized(testTimings.toList), useColors = false)

  /** Echo the concise summary to stdout; local runs also log reproduction instructions. */
  override def echoSummary(): Unit = {
    flushTestTimings()
    val currentSummary = summary
    val failures = currentSummary.failures
    val hasResults = currentSummary.passed + failures.size + currentSummary.skipped > 0
    TestReporter.writeFailedTests(failures.map(_.title).distinct)

    if hasResults then println(VulpixConsole.renderSummary(currentSummary, VulpixConsole.colorsEnabled))

    val overallTimings =
      if progressEnabled then VulpixConsole.renderSlowestOverall(synchronized(testTimings.toList), VulpixConsole.colorsEnabled)
      else ""
    if overallTimings.nonEmpty then println(overallTimings)

    if !Properties.isRunByCI && failures.nonEmpty then
      println {
        s"""|--------------------------------------------------------------------------------
            |Note - reproduction instructions have been dumped to log file:
            |    ${TestReporter.logPath}
            |--------------------------------------------------------------------------------""".stripMargin
      }

    val reproduction = reproductionText
    if reproduction.nonEmpty && !Properties.isRunByCI then echoToLog(List(reproduction))
  }

  override private[vulpix] def progressEnabled: Boolean = pulse
  override private[vulpix] def groupAnnouncementsEnabled: Boolean = announceGroups

  override private[vulpix] def reportTestTimings(timings: Iterable[VulpixConsole.TestTiming]): Unit = synchronized {
    testTimings ++= timings
    pendingTestTimings ++= timings
  }

  override private[vulpix] def beginTestGroups(groups: Set[String]): Unit =
    if progressEnabled || groupAnnouncementsEnabled then {
      val started = synchronized {
        val started = groups -- currentTestGroups
        currentTestGroups = groups
        started
      }
      if progressEnabled then emitTestTimings(drainTestTimingsExcept(groups))
      if groupAnnouncementsEnabled && started.nonEmpty then
        println(VulpixConsole.renderGroupStarts(started, VulpixConsole.colorsEnabled))
    }

  override private[vulpix] def flushTestTimings(): Unit = {
    synchronized { currentTestGroups = Set.empty }
    if progressEnabled then emitTestTimings(drainTestTimingsExcept(Set.empty))
  }

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

  override def echoToLog(it: Iterable[String]): Unit =
    if it.nonEmpty then synchronized {
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
