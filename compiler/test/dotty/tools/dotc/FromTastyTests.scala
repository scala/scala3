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
      // compileTasty("../tests/pos/t115.scala", defaultOptions) +
      // compileTasty("../tests/pos/t2127.scala", defaultOptions) +
      // compileTasty("../tests/pos/i1570.scala", defaultOptions) +
      // compileTasty("../tests/pos/t1279a.scala", defaultOptions) +
      // compileTasty("../tests/pos/i3129.scala", defaultOptions) +
      // compileTasty("../tests/pos/i2250.scala", defaultOptions) +
      // compileTasty("../tests/pos/i966.scala", defaultOptions) +
      // compileTasty("../tests/pos/t0049.scala", defaultOptions) +
      // compileTasty("../tests/pos/i0306.scala", defaultOptions) +
      // compileTasty("../tests/pos/t0055.scala", defaultOptions) +
      // compileTasty("../tests/pos/i2397.scala", defaultOptions) +
      // compileTasty("../tests/pos/i1365.scala", defaultOptions) +
      // compileTasty("../tests/pos/t1957.scala", defaultOptions) +
      // compileTasty("../tests/pos/i1756.scala", defaultOptions) +
      // compileTasty("../tests/pos/i3130d.scala", defaultOptions) +
      // compileTasty("../tests/pos/i1990a.scala", defaultOptions) +
      // compileTasty("../tests/pos/i996.scala", defaultOptions) +
      // compileTasty("../tests/pos/companions.scala", defaultOptions) +
      // compileTasty("../tests/pos/i1812.scala", defaultOptions) +
      // compileTasty("../tests/pos/i2944.scala", defaultOptions) +
      // compileTasty("../tests/pos/i2468.scala", defaultOptions) +
      // compileTasty("../tests/pos/i2300.scala", defaultOptions) +
      // compileTasty("../tests/pos/i1990.scala", defaultOptions) +
      // compileTasty("../tests/pos/i1812b.scala", defaultOptions) +
      // compileTasty("../tests/pos/t2405.scala", defaultOptions) +
      // compileTasty("../tests/pos/supercalls.scala", defaultOptions) +
      // compileTasty("../tests/pos/hklub0.scala", defaultOptions) +
      // compileTasty("../tests/pos/i3130a.scala", defaultOptions) +
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
      // compileTasty("../tests/run/t3613.scala", defaultOptions) +
      // compileTasty("../tests/run/i1569.scala", defaultOptions) +
      // compileTasty("../tests/run/i2337.scala", defaultOptions) +
      // compileTasty("../tests/run/t2127.scala", defaultOptions) +
      // compileTasty("../tests/run/scala2trait-lazyval.scala", defaultOptions) +
      // compileTasty("../tests/run/t6666a.scala", defaultOptions) +
      // compileTasty("../tests/run/t3452f.scala", defaultOptions) +
      // compileTasty("../tests/run/t6957.scala", defaultOptions) +
      // compileTasty("../tests/run/inlinedAssign.scala", defaultOptions) +
      // compileTasty("../tests/run/bridges.scala", defaultOptions) +
      // compileTasty("../tests/run/t8002.scala", defaultOptions) +
      // compileTasty("../tests/run/t6506.scala", defaultOptions) +
      // compileTasty("../tests/run/enum-approx.scala", defaultOptions) +
      // compileTasty("../tests/run/i2337b.scala", defaultOptions) +
      // compileTasty("../tests/run/array-addition.scala", defaultOptions) +
      // compileTasty("../tests/run/t1909c.scala", defaultOptions) +
      // compileTasty("../tests/run/i2163.scala", defaultOptions) +
      // compileTasty("../tests/run/t8395.scala", defaultOptions) +
      // compileTasty("../tests/run/view-iterator-stream.scala", defaultOptions) +
      // compileTasty("../tests/run/Course-2002-13.scala", defaultOptions) +
      // compileTasty("../tests/run/NestedClasses.scala", defaultOptions) +
      // compileTasty("../tests/run/inlineForeach.scala", defaultOptions) +
      // compileTasty("../tests/run/i1990b.scala", defaultOptions) +
      // compileTasty("../tests/run/t3048.scala", defaultOptions) +
      // compileTasty("../tests/run/patmat-bind-typed.scala", defaultOptions) +
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
