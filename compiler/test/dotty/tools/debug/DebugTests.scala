package dotty.tools.debug

import dotty.Properties
import dotty.tools.dotc.reporting.TestReporter
import dotty.tools.io.JFile
import dotty.tools.vulpix.*
import org.junit.Test

import scala.concurrent.duration.*

class DebugTests:
  import DebugTests.*
  @Test def debug: Unit =
    implicit val testGroup: TestGroup = TestGroup("debug")
    compileFilesInDir("tests/debug", TestConfiguration.defaultOptions).checkDebug()

end DebugTests

object DebugTests extends ParallelTesting:
  def maxDuration = 45.seconds
  def numberOfSlaves = Runtime.getRuntime().availableProcessors()
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter
  def updateCheckFiles: Boolean = Properties.testsUpdateCheckfile
  def failedTests = TestReporter.lastRunFailedTests
  override def debugMode = true

  implicit val summaryReport: SummaryReporting = new SummaryReport

  extension (test: CompilationTest)
    private def checkDebug()(implicit summaryReport: SummaryReporting): test.type =
      import test.*
      checkPass(new DebugTest(targets, times, threadLimit, shouldFail || shouldSuppressOutput), "Debug")

  private final class DebugTest(testSources: List[TestSource], times: Int, threadLimit: Option[Int], suppressAllOutput: Boolean)(implicit summaryReport: SummaryReporting)
    extends RunTest(testSources, times, threadLimit, suppressAllOutput):

    override def onSuccess(testSource: TestSource, reporters: Seq[TestReporter], logger: LoggedRunnable) =
      verifyDebug(testSource.outDir, testSource, countWarnings(reporters), reporters, logger)

    private def verifyDebug(dir: JFile, testSource: TestSource, warnings: Int, reporters: Seq[TestReporter], logger: LoggedRunnable) =
      if Properties.testsNoRun then addNoRunWarning()
      else runMain(testSource.runClassPath) match
        case Success(output) => ()
        case Failure(output) =>
          if output == "" then
            echo(s"Test '${testSource.title}' failed with no output")
          else
            echo(s"Test '${testSource.title}' failed with output:")
            echo(output)
          failTestSource(testSource)
        case Timeout =>
          echo("failed because test " + testSource.title + " timed out")
          failTestSource(testSource, TimeoutFailure(testSource.title))

  end DebugTest
