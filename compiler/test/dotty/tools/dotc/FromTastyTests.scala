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
        "macro-deprecate-dont-touch-backquotedidents.scala",
        "t247.scala",

        // Wrong number of arguments
        "i3130b.scala",

        // Class not found
        "i3130a.scala",

        // Owner discrepancy for refinements
        "lambdalift-1.scala",

        // Cannot merge members
        "depfuntype.scala",

        // Type miss match after unpickling
        "hklub0.scala",

        // Missing position
        "t1203a.scala",
        "t2260.scala",
        "t4579.scala",
        "tcpoly_ticket2096.scala",
        "i2345.scala",
        "t4731.scala",
        "spec-super.scala",
        "spec-sparsearray-old.scala",
        "collections_1.scala",

        // Anonymous method not defined
        "i3067.scala",

        // Infinite compilation
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

         "t7223.scala",
         "t5428.scala",

         // Missing position
         "Course-2002-13.scala",
         "bridges.scala",
         "i2337.scala",
         "i2337b.scala",
         "scala2trait-lazyval.scala",
         "t3452f.scala",

         // Closure type miss match
         "eff-dependent.scala",

         // Unpickling tree without owner
         "patmat-bind-typed.scala",
         "t8395.scala",

         // Issue unpickling universes
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

         // Issue with JFunction1$mcI$sp/T
         "erased-15.scala",
         "erased-17.scala",
         "erased-20.scala",
         "erased-21.scala",
         "erased-23.scala",
         "erased-value-class.scala",
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
