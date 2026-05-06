package dotty
package tools
package dotc

import dotty.tools.vulpix.*
import reporting.TestReporter

import scala.concurrent.duration.*
import scala.language.unsafeNulls

import java.io.{File => JFile}
import org.junit.{AfterClass, Test}

class BestEffortOptionsTests {
  import ParallelTesting.*
  import vulpix.TestConfiguration.*
  import BestEffortOptionsTests.*

  // Since TASTy and beTASTy files are read in a lazy manner (only when referenced by the source .scala file)
  // we test by using the "-from-tasty" option. This guarantees that the tasty files will be read
  // (and that the Best Effort TASTy reader will be tested), but we unfortunately skip the useful
  // interactions a tree derived from beTASTy could have with other frontend phases.
  @Test def negTestFromBestEffortTasty: Unit =
    // Can be reproduced with
    // > sbt
    // > scalac -Ybest-effort -Xsemanticdb <source>
    // > scalac -from-tasty -Ywith-best-effort-tasty META_INF/best-effort/<betasty>

    given TestGroup = TestGroup("negTestFromBestEffortTasty")
    compileBestEffortTastyInDir(s"tests${JFile.separator}neg", bestEffortBaselineOptions,
      picklingFilter = FileFilter.exclude(TestSources.negBestEffortPicklingExcludelisted),
      unpicklingFilter = FileFilter.exclude(TestSources.negBestEffortUnpicklingExcludelisted)
    ).checkNoCrash()

  // Tests an actual use case of this compilation mode, where symbol definitions of the downstream
  // projects depend on the best effort tasty files generated with the Best Effort dir option
  @Test def bestEffortIntegrationTest: Unit =
    given TestGroup = TestGroup("bestEffortIntegrationTests")
    compileBestEffortIntegration(s"tests${JFile.separator}best-effort", bestEffortBaselineOptions)
      .noCrashWithCompilingDependencies()
}

object BestEffortOptionsTests extends ParallelTesting {
  def maxDuration = 45.seconds
  def numberOfWorkers = Runtime.getRuntime.availableProcessors()
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
