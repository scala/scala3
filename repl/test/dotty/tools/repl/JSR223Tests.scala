package dotty.tools
package repl

import org.junit.Test
import org.junit.AfterClass

import vulpix.*

import scala.concurrent.duration._

final class JSR223Tests:

  import JSR223Tests.{*, given}

  @Test def filetests: Unit =
    given TestGroup = TestGroup("runWithCompiler")
    compileFilesInDir("test-resources/jsr223", TestConfiguration.withReplOptions)
      .checkRuns()
  end filetests

end JSR223Tests

object JSR223Tests extends ParallelTesting:

  given report: SummaryReporting = new SummaryReport

  def maxDuration = 100.seconds
  def numberOfWorkers = Runtime.getRuntime().availableProcessors()
  def safeMode = dotty.Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = dotty.Properties.testsFilter
  def updateCheckFiles: Boolean = dotty.Properties.testsUpdateCheckfile
  def failedTests = dotc.reporting.TestReporter.lastRunFailedTests

  @AfterClass def tearDown(): Unit = {
    super.cleanup()
    report.echoSummary()
  }

end JSR223Tests