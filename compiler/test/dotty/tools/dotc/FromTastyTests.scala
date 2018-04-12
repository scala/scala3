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
    val (step1, step2, step3) = compileTastyInDir("tests/pos", defaultOptions,
      blacklist = Set(
        // Wrong number of arguments (only on bootstrapped)
        "i3130b.scala",

        // Missing position
        "collections_1.scala",

        // MatchError in SymDenotation.sourceModule on a ThisType
        "t3612.scala",
      )
    )
    step1.checkCompile() // Compile all files to generate the class files with tasty
    step2.checkCompile() // Compile from tasty
    step3.checkCompile() // Decompile from tasty
    (step1 + step2 + step3).delete()
  }

  @Test def runTestFromTasty: Unit = {
    // Can be reproduced with
    // > sbt
    // > dotc -Ythrough-tasty -Ycheck:all <source>
    // > dotr Test

    implicit val testGroup: TestGroup = TestGroup("runTestFromTasty")
    val (step1, step2, step3) = compileTastyInDir("tests/run", defaultOptions,
       blacklist = Set(
         // Closure type miss match
         "eff-dependent.scala",
       )
    )
    step1.checkCompile() // Compile all files to generate the class files with tasty
    step2.checkRuns() // Compile from tasty and run the result
    step3.checkCompile() // Decompile from tasty
    (step1 + step2 + step3).delete()
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
