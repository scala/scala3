package dotty
package tools
package dotc

import org.junit.{ Test, BeforeClass, AfterClass }
import org.junit.experimental.categories.Category

import scala.concurrent.duration._
import vulpix._

@Category(Array(classOf[ScalaJSCompilationTests]))
class ScalaJSCompilationTests extends ParallelTesting {
  import ParallelTesting._
  import TestConfiguration._
  import ScalaJSCompilationTests._
  import CompilationTest.aggregateTests

  // Test suite configuration --------------------------------------------------

  def maxDuration = 60.seconds
  def numberOfSlaves = 5
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter
  def updateCheckFiles: Boolean = Properties.testsUpdateCheckfile

  // Negative tests ------------------------------------------------------------

  @Test def negScalaJS: Unit = {
    implicit val testGroup: TestGroup = TestGroup("negScalaJS")
    aggregateTests(
      compileFilesInDir("tests/neg-scalajs", scalaJSOptions),
    ).checkExpectedErrors()
  }

  // Run tests -----------------------------------------------------------------

  override protected def shouldSkipTestSource(testSource: TestSource): Boolean =
    testSource.allToolArgs.get(ToolName.ScalaJS).exists(_.contains("--skip"))

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

  @Test def runScalaJS: Unit = {
    implicit val testGroup: TestGroup = TestGroup("runScalaJS")
    aggregateTests(
      compileFilesInDir("tests/run", scalaJSOptions),
    ).checkRuns()
  }
}

object ScalaJSCompilationTests {
  implicit val summaryReport: SummaryReporting = new SummaryReport
  @AfterClass def cleanup(): Unit = summaryReport.echoSummary()
}
