package dotty
package tools
package vulpix

import scala.collection.mutable
import dotc.reporting.TestReporter

trait SummaryReporting {
  def reportFailed(): Unit
  def reportPassed(): Unit
  def addFailedTest(msg: String): Unit
  def addReproduceInstruction(instr: String): Unit
  def addStartingMessage(msg: String): Unit
  def addCleanup(f: () => Unit): Unit
  def echoSummary(): Unit
}

final class NoSummaryReport extends SummaryReporting {
  def reportFailed(): Unit = ()
  def reportPassed(): Unit = ()
  def addFailedTest(msg: String): Unit = ()
  def addReproduceInstruction(instr: String): Unit = ()
  def addStartingMessage(msg: String): Unit = ()
  def addCleanup(f: () => Unit): Unit = ()
  def echoSummary(): Unit = ()
}

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

    failedTests.map(x => "    " + x).foreach(rep.append)

    // If we're compiling locally, we don't need instructions on how to
    // reproduce failures
    if (isInteractive) {
      println(rep.toString)
      if (failed > 0) println {
        """|
           |----------------------------------------------------------
           |Note: reproduction instructed have been dumped to log file
           |----------------------------------------------------------""".stripMargin
      }
    }

    rep += '\n'

    reproduceInstructions.foreach(rep.append)

    // If we're on the CI, we want everything
    if (!isInteractive) println(rep.toString)

    TestReporter.writeToLog(rep.toString)

    // Perform cleanup callback:
    if (cleanUps.nonEmpty) cleanUps.foreach(_.apply())
  }
}

object SummaryReport {
  val isInteractive = Properties.testsInteractive && !Properties.isRunByDrone
}
