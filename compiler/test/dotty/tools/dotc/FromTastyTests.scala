package dotty
package tools
package dotc

import org.junit.{AfterClass, Test}
import vulpix._

import scala.concurrent.duration._

class FromTastyTests extends ParallelTesting {
  import TestConfiguration._
  import FromTastyTests._

  // Test suite configuration --------------------------------------------------

  def maxDuration = 30.seconds
  def numberOfSlaves = 5
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter


  @Test def posTestFromTasty: Unit = {
    implicit val testGroup: TestGroup = TestGroup("posTestFromTasty")
    val (step1, step2) = {
      // compileTastyInDir("../tests/pos", defaultOptions) + // FIXME
      compileTastyInDir("../tests/pos-from-tasty", defaultOptions) +
      compileTasty("../tests/pos-from-tasty/simpleClass.scala", defaultOptions)
    }
    step1.checkCompile() // Compile all files to generate the class files with tasty
    step2.checkCompile() // Compile from tasty
    (step1 + step2).delete()
  }

  @Test def runTestFromTasty: Unit = {
    implicit val testGroup: TestGroup = TestGroup("runTestFromTasty")
    val (step1, step2) = {
      // compileTastyInDir("../tests/run", defaultOptions) + // FIXME
      compileTastyInDir("../tests/run-from-tasty", defaultOptions) +
      compileTasty("../tests/run/t493.scala", defaultOptions)
    }
    step1.checkCompile() // Compile all files to generate the class files with tasty
    step2.checkRuns() // Compile from tasty and run the result
    (step1 + step2).delete()
  }

  private implicit class tastyCompilationTuples(tup: (CompilationTest, CompilationTest)) {
    def +(that: (CompilationTest, CompilationTest)): (CompilationTest, CompilationTest) =
      (tup._1 + that._1, tup._2 + that._2)
  }
}

object FromTastyTests {
  implicit val summaryReport: SummaryReporting = new SummaryReport
  @AfterClass def cleanup(): Unit = summaryReport.echoSummary()
}
