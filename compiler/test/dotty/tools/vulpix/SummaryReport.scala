package dotty
package tools
package vulpix

import scala.collection.mutable
import dotc.reporting.TestReporter

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
  def addFailedTest(msg: String): Unit

  /** Add instructions to reproduce the error */
  def addReproduceInstruction(instr: String): Unit

  /** Add a message that will be issued in the beginning of the summary */
  def addStartingMessage(msg: String): Unit

  /** Add a cleanup hook to be run upon completion */
  def addCleanup(f: () => Unit): Unit

  /** Echo the summary report to the appropriate locations */
  def echoSummary(): Unit
}

/** A summary report that doesn't do anything */
final class NoSummaryReport extends SummaryReporting {
  def reportFailed(): Unit = ()
  def reportPassed(): Unit = ()
  def addFailedTest(msg: String): Unit = ()
  def addReproduceInstruction(instr: String): Unit = ()
  def addStartingMessage(msg: String): Unit = ()
  def addCleanup(f: () => Unit): Unit = ()
  def echoSummary(): Unit = ()
}

/** A summary report that logs to both stdout and the `TestReporter.logWriter`
 *  which outputs to a log file in `./testlogs/`
 */
final class SummaryReport extends SummaryReporting {

  private val startingMessages = mutable.ArrayBuffer.empty[String]
  private val failedTests = mutable.ArrayBuffer.empty[String]
  private val reproduceInstructions = mutable.ArrayBuffer.empty[String]
  private val cleanUps = mutable.ArrayBuffer.empty[() => Unit]

  private[this] var passed = 0
  private[this] var failed = 0

  def reportFailed(): Unit =
    failed += 1

  def reportPassed(): Unit =
    passed += 1

  def addFailedTest(msg: String): Unit =
    failedTests.append(msg)

  def addReproduceInstruction(instr: String): Unit =
    reproduceInstructions.append(instr)

  def addStartingMessage(msg: String): Unit =
    startingMessages.append(msg)

  def addCleanup(f: () => Unit): Unit =
    cleanUps.append(f)

  /** Both echoes the summary to stdout and prints to file */
  def echoSummary(): Unit = {
    import SummaryReport._

    val rep = new StringBuilder
    rep.append(
      s"""|
          |================================================================================
          |Test Report
          |================================================================================
          |
          |$passed passed, $failed failed, ${passed + failed} total
          |""".stripMargin
    )

    startingMessages.foreach(rep.append)

    failedTests.map(x => s"    $x\n").foreach(rep.append)

    // If we're compiling locally, we don't need instructions on how to
    // reproduce failures
    if (isInteractive) {
      println(rep.toString)
      if (failed > 0) println {
        s"""|
            |--------------------------------------------------------------------------------
            |Note - reproduction instructions have been dumped to log file:
            |    ${TestReporter.logPath}
            |--------------------------------------------------------------------------------""".stripMargin
      }
    }

    rep += '\n'

    reproduceInstructions.foreach(rep.append)

    // If we're on the CI, we want everything
    if (!isInteractive) println(rep.toString)

    TestReporter.logPrintln(rep.toString)

    // Perform cleanup callback:
    if (cleanUps.nonEmpty) cleanUps.foreach(_.apply())
  }
}

object SummaryReport {
  val isInteractive = Properties.testsInteractive && !Properties.isRunByDrone
}
