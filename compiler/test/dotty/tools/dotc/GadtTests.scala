package dotty
package tools
package dotc

import org.junit.{AfterClass, Test}
import vulpix._

import scala.concurrent.duration._
import java.io.{File => JFile}

class GadtTests extends ParallelTesting {
  import TestConfiguration._
  import GadtTests._

  // Test suite configuration --------------------------------------------------

  def maxDuration = 30.seconds
  def numberOfSlaves = 5
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter

  @Test def compileGadtTests: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compileGadtTests")
    compileFilesInDir("tests/gadt", defaultOptions).checkCompile()
  }

  @Test def compileGadtNeg: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compileGadtNeg")
    compileFilesInDir("tests/gadt-neg", defaultOptions).checkExpectedErrors()
  }
}

object GadtTests {
  implicit val summaryReport: SummaryReporting = new SummaryReport
  @AfterClass def cleanup(): Unit = summaryReport.echoSummary()
}
