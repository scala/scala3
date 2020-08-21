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
}

object ScalaJSCompilationTests {
  implicit val summaryReport: SummaryReporting = new SummaryReport
  @AfterClass def cleanup(): Unit = summaryReport.echoSummary()
}
