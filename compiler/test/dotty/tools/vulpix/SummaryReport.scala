package dotty
package tools
package vulpix

import dotc.reporting.TestReporter
import java.util.concurrent.ConcurrentLinkedDeque

/** `SummaryReporting` can be used by unit tests by utilizing `@AfterClass` to
 *  call `echoSummary`
 *
 *  This is used in vulpix by passing the companion object's `SummaryReporting`
 *  to each test, the `@AfterClass def` then calls the `SummaryReport`'s
 *  `echoSummary` method in order to dump the summary to both stdout and a log
 *  file
 */
trait SummaryReporting {
  /** Report a failed test */
  def reportFailed(): Unit

  /** Report a test as passing */
  def reportPassed(): Unit

  /** Add the name of the failed test */
  def addFailedTest(msg: FailedTestInfo): Unit

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
  override def reportFailed(): Unit = ()
  override def reportPassed(): Unit = ()
  override def addFailedTest(msg: FailedTestInfo): Unit = ()
  override def addSkippedTest(msg: FailedTestInfo): Unit = ()
  override def addReproduceInstruction(instr: String): Unit = ()
  override def echoSummary(): Unit = ()
  override def echoToLog(it: Iterable[String]): Unit = ()
}

/** A summary report that logs to both stdout and the `TestReporter.logWriter`
 *  which outputs to a log file in `./testlogs/`
 */
final class SummaryReport extends SummaryReporting {
  import scala.jdk.CollectionConverters.*

  private val failedTests = new ConcurrentLinkedDeque[FailedTestInfo]
  private val skippedTests = new ConcurrentLinkedDeque[FailedTestInfo]
  private val reproduceInstructions = new ConcurrentLinkedDeque[String]

  private var passed = 0
  private var failed = 0

  override def reportFailed(): Unit =
    failed += 1

  override def reportPassed(): Unit =
    passed += 1

  override def addFailedTest(msg: FailedTestInfo): Unit =
    failedTests.add(msg)

  override def addSkippedTest(msg: FailedTestInfo): Unit =
    skippedTests.add(msg)

  override def addReproduceInstruction(instr: String): Unit =
    reproduceInstructions.add(instr)

  /** Both echoes the summary to stdout and prints to file */
  override def echoSummary(): Unit = {
    val rep = new StringBuilder
    if failed == 0 && failedTests.isEmpty then
      rep.append(s"== Vulpix Test Report: $passed suites passed, no failures (${skippedTests.size} skipped) ==")
    else
      rep.append(
        s"""|
            |================================================================================
            |Vulpix Test Report
            |================================================================================
            |
            |$passed suites passed, $failed failed, ${passed + failed} total
            |""".stripMargin
      )
      failedTests.asScala.map(x => s"    ${x.title}${x.extra}\n").foreach(rep.append)
      TestReporter.writeFailedTests(failedTests.asScala.toList.map(_.title))
      if !skippedTests.isEmpty then
        rep.append("Skipped: " + skippedTests.asScala.map(_.title).mkString(", "))

    // If we're on the CI, we want reproduction instructions; otherwise, we just need a pointer to the log file.
    if Properties.isRunByCI then
      if !reproduceInstructions.isEmpty then
        rep += '\n'
        reproduceInstructions.asScala.foreach(rep.append)
    else
      if failed > 0 then rep.append(
        s"""|
            |--------------------------------------------------------------------------------
            |Note - reproduction instructions have been dumped to log file:
            |    ${TestReporter.logPath}
            |--------------------------------------------------------------------------------""".stripMargin
      ).append('\n')

    println(rep.toString)
    TestReporter.logPrintln(rep.toString)
  }

  private def removeColors(msg: String): String =
    msg.replaceAll("\u001b\\[.*?m", "")

  override def echoToLog(it: Iterable[String]): Unit = {
    it.foreach(msg => TestReporter.logPrint(removeColors(msg)))
    TestReporter.logFlush()
  }
}

case class FailedTestInfo(title: String, extra: String)
