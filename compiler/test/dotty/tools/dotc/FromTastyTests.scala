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
    // Can be reproduced with
    // > sbt
    // > dotc -Ythrough-tasty -Ycheck:all <source>

    implicit val testGroup: TestGroup = TestGroup("posTestFromTasty")
    compileTastyInDir("tests/pos", defaultOptions,
      blacklist = FromTastySources.posFromTastyBlacklisted.toSet,
      decompilationBlacklist = FromTastySources.posDecompilationBlacklisted.toSet,
      recompilationWhitelist = FromTastySources.posRecompilationWhitelist.toSet
    ).checkCompile()
  }

  @Test def runTestFromTasty: Unit = {
    // Can be reproduced with
    // > sbt
    // > dotc -Ythrough-tasty -Ycheck:all <source>
    // > dotr Test

    implicit val testGroup: TestGroup = TestGroup("runTestFromTasty")
    compileTastyInDir("tests/run", defaultOptions,
      blacklist = FromTastySources.runFromTastyBlacklisted.toSet,
      decompilationBlacklist = FromTastySources.runDecompilationBlacklisted.toSet,
      recompilationWhitelist = FromTastySources.runRecompilationWhitelist.toSet
    ).checkRuns()
  }
}

object FromTastyTests {
  implicit val summaryReport: SummaryReporting = new SummaryReport
  @AfterClass def cleanup(): Unit = summaryReport.echoSummary()
}
