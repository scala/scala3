package dotty
package tools
package dotc

import org.junit.{AfterClass, Test}
import reporting.TestReporter
import vulpix.*

import scala.concurrent.duration.*

class FromTastyTests {
  import TestConfiguration.*
  import FromTastyTests.*

  @Test def posTestFromTasty: Unit = {
    // Can be reproduced with
    // > sbt
    // > scalac -Ythrough-tasty -Ycheck:all <source>

    implicit val testGroup: TestGroup = TestGroup("posTestFromTasty")
    compileTastyInDir("tests/pos", defaultOptions,
      fromTastyFilter = FileFilter.exclude(TestSources.posFromTastyExcludelisted)
    ).checkCompile()
  }

  @Test def runTestFromTasty: Unit = {
    // Can be reproduced with
    // > sbt
    // > scalac -Ythrough-tasty -Ycheck:all <source>
    // > scala Test

    implicit val testGroup: TestGroup = TestGroup("runTestFromTasty")
    compileTastyInDir("tests/run", defaultOptions,
      fromTastyFilter = FileFilter.exclude(TestSources.runFromTastyExcludelisted)
    ).checkRuns()
  }
}

object FromTastyTests extends ParallelTesting {
  // Test suite configuration --------------------------------------------------

  def maxDuration = 30.seconds
  def numberOfWorkers = Runtime.getRuntime().availableProcessors()
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter
  def updateCheckFiles: Boolean = Properties.testsUpdateCheckfile
  def failedTests = TestReporter.lastRunFailedTests

  implicit val summaryReport: SummaryReporting = new SummaryReport
  @AfterClass def tearDown(): Unit = {
    super.cleanup()
    summaryReport.echoSummary()
  }
}
