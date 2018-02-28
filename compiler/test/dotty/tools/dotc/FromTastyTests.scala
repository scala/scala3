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

        // Compiles wrong class
        "simpleClass.scala",

        // Wrong number of arguments
        "i3130b.scala",

        // Owner discrepancy for refinements
        "NoCyclicReference.scala",
        "i1795.scala",
        "lambdalift-1.scala",

        // Cannot merge members
        "depfuntype.scala",

        // NPE in HKLambda.computeHash
        "i2888.scala",
        "i974.scala",
        "t3800.scala",

        // Type miss match after unpickling
        "i2944.scala",
        "t8023.scala",
        "hklub0.scala",
        "i1365.scala",
        "t6205.scala",

        // Missing position
        "i3000.scala",
        "t1203a.scala",
        "t2260.scala",
        "t4579.scala",
        "tcpoly_ticket2096.scala",
        "t247.scala",
        "i2345.scala",
        "i0306.scala",
        "t4731.scala",
        "spec-super.scala",
        "spec-sparsearray-old.scala",
        "collections_1.scala",

        // cyclic type references
        "i536.scala",
        "cyclics-pos.scala",

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

         "t3613.scala",
         "t7223.scala",
         "t7899-regression.scala",

         // Missing position
         "Course-2002-13.scala",
         "bridges.scala",
         "i2337.scala",
         "i2337b.scala",
         "enum-approx.scala",
         "inlineForeach.scala",
         "scala2trait-lazyval.scala",
         "t3452f.scala",
         "t5428.scala",

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
         "unused-15.scala",
         "unused-17.scala",
         "unused-20.scala",
         "unused-21.scala",
         "unused-23.scala",
         "unused-value-class.scala",
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
