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


  // @Test def posTestFromTasty: Unit = {
  //   // Can be reproduced with
  //   // > sbt
  //   // > dotc -Ythrough-tasty -Ycheck:all <source>

  //   implicit val testGroup: TestGroup = TestGroup("posTestFromTasty")
  //   compileTastyInDir("tests/pos", defaultOptions,
  //     fromTastyFilter = FileFilter.exclude(TestSources.posFromTastyBlacklisted),
  //     decompilationFilter = FileFilter.exclude(TestSources.posDecompilationBlacklisted),
  //     recompilationFilter = FileFilter.include(TestSources.posRecompilationWhitelist)
  //   ).checkCompile()
  // }

  @Test def compileGadtTests: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compileGadtTests")
    compileFilesInDir("tests/gadt+noCheckOptions", TestFlags(basicClasspath, noCheckOptions)).checkCompile()
  }

  @Test def compileGadtCheckOptionsTests: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compileGadtCheckOptionsTests")
    compileFilesInDir("tests/gadt+checkOptions", TestFlags(basicClasspath, noCheckOptions ++ checkOptions)).checkCompile()
  }

  @Test def compileGadtDefaultOptionsTests: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compileGadtDefaultOptionsTests")
    compileFilesInDir("tests/gadt+defaultOptions", defaultOptions).checkCompile()
  }
}

object GadtTests {
  implicit val summaryReport: SummaryReporting = new SummaryReport
  @AfterClass def cleanup(): Unit = summaryReport.echoSummary()
}
