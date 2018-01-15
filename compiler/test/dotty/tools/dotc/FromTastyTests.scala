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
    val (step1, step2) = compileTastyInDir("../tests/pos", defaultOptions,
      blacklist = Set(
        "NoCyclicReference.scala",
        "depfuntype.scala",
        "hklub0.scala",
        "i0306.scala",
        "i1365.scala",
        "i1795.scala",
        "i2345.scala",
        "i2888.scala",
        "i2944.scala",
        "i3000.scala",
        "i536.scala",
        "i974.scala",
        "t1203a.scala",
        "t2260.scala",
        "t3612.scala", // Test never finishes
        "t3800.scala",
        "t4579.scala",
        "t8023.scala",
        "tcpoly_ticket2096.scala",
        "t247.scala",
        "i3067.scala",
      )
    )
    step1.checkCompile() // Compile all files to generate the class files with tasty
    step2.checkCompile() // Compile from tasty
    (step1 + step2).delete()
  }

  @Test def runTestFromTasty: Unit = {
    // Can be reproduced with
    // > sbt
    // > dotc -Ythrough-tasty -Ycheck:all <source>
    // > dotr Test

    implicit val testGroup: TestGroup = TestGroup("runTestFromTasty")
    val (step1, step2) = compileTastyInDir("../tests/run", defaultOptions,
       blacklist = Set(
         "Course-2002-13.scala",
         "bridges.scala",
         "eff-dependent.scala",
         "enum-approx.scala",
         "i2337.scala",
         "i2337b.scala",
         "inlineForeach.scala",
         "patmat-bind-typed.scala",
         "phantom-decls-1.scala",
         "phantom-decls-3.scala",
         "phantom-decls-5.scala",
         "phantom-hk-1.scala",
         "phantom-hk-2.scala",
         "phantom-in-value-class.scala",
         "phantom-methods-3.scala",
         "phantom-methods-4.scala",
         "phantom-poly-1.scala",
         "phantom-poly-2.scala",
         "phantom-poly-3.scala",
         "phantom-poly-4.scala",
         "scala2trait-lazyval.scala",
         "t3452f.scala",
         "t8395.scala",
         "t3613.scala",
       )
    )
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
