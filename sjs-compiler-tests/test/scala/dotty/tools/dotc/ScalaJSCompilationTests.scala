package dotty
package tools
package dotc

import org.junit.{ Test, BeforeClass, AfterClass }
import org.junit.experimental.categories.Category

import scala.concurrent.duration._
import reporting.TestReporter
import vulpix._
import org.junit.Ignore

@Category(Array(classOf[ScalaJSCompilationTests]))
class ScalaJSCompilationTests {
  import ParallelTesting._
  import TestConfiguration._
  import ScalaJSCompilationTests._
  import CompilationTest.aggregateTests

  // Negative tests ------------------------------------------------------------

  @Test def negScalaJS: Unit = {
    implicit val testGroup: TestGroup = TestGroup("negScalaJS")
    aggregateTests(
      compileFilesInDir("tests/neg-scalajs", scalaJSOptions),
    ).checkExpectedErrors()
  }

  @Test def runScalaJS: Unit = {
    implicit val testGroup: TestGroup = TestGroup("runScalaJS")
    aggregateTests(
      compileFilesInDir("tests/run", scalaJSOptions),
    ).checkRuns()
  }
}

object ScalaJSCompilationTests extends ParallelTesting {
  implicit val summaryReport: SummaryReporting = new SummaryReport

  // Test suite configuration --------------------------------------------------
  def maxDuration = 60.seconds
  def numberOfWorkers = 5
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter
  def updateCheckFiles: Boolean = Properties.testsUpdateCheckfile
  def failedTests = TestReporter.lastRunFailedTests

  @AfterClass def tearDown(): Unit =
    cleanup()
    summaryReport.echoSummary()

  // Run tests -----------------------------------------------------------------

  override protected def shouldSkipTestSource(testSource: TestSource): Boolean =
    testSource.allToolArgs.get(ToolName.ScalaJS).exists(_.contains("--skip"))
    || super.shouldSkipTestSource(testSource)

  override protected def testPlatform: TestPlatform = TestPlatform.ScalaJS

  override def runMain(classPath: String, toolArgs: ToolArgs)(implicit summaryReport: SummaryReporting): Status =
    import scala.concurrent.ExecutionContext.Implicits.global

    val scalaJSOptions = toolArgs.getOrElse(ToolName.ScalaJS, Nil)

    try
      val useCompliantSemantics = scalaJSOptions.contains("--compliant-semantics")
      val sjsCode = ScalaJSLink.link(classPath, useCompliantSemantics)
      JSRun.runJSCode(sjsCode)
    catch
      case t: Exception =>
        val writer = new java.io.StringWriter()
        t.printStackTrace(new java.io.PrintWriter(writer))
        Failure(writer.toString())
  end runMain
}
