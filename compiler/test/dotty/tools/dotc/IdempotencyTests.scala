package dotty
package tools
package dotc

import java.io.{File => JFile}
import java.nio.file.{Files, Path, Paths}

import org.junit.Assume.assumeTrue
import org.junit.{AfterClass, Test}
import org.junit.experimental.categories.Category

import scala.concurrent.duration._
import vulpix._


class IdempotencyTests extends ParallelTesting {
  import TestConfiguration._
  import IdempotencyTests._
  import CompilationTest.aggregateTests

  // Test suite configuration --------------------------------------------------

  def maxDuration = 30.seconds
  def numberOfSlaves = 5
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter
  def updateCheckFiles: Boolean = Properties.testsUpdateCheckfile

  @Category(Array(classOf[SlowTests]))
  @Test def idempotency: Unit = {
    implicit val testGroup: TestGroup = TestGroup("idempotency")
    val opt = defaultOptions

    val posIdempotency = aggregateTests(
      compileFilesInDir("tests/pos", opt)(TestGroup("idempotency/posIdempotency1")),
      compileFilesInDir("tests/pos", opt)(TestGroup("idempotency/posIdempotency2")),
    )

    val orderIdempotency = {
      val tests =
        for {
          testDir <- new JFile("tests/order-idempotency").listFiles() if testDir.isDirectory
        } yield {
          val sources = TestSources.sources(testDir.toPath)
          aggregateTests(
            compileList(testDir.getName, sources, opt)(TestGroup("idempotency/orderIdempotency1")),
            compileList(testDir.getName, sources.reverse, opt)(TestGroup("idempotency/orderIdempotency2"))
          )
        }
      aggregateTests(tests: _*)
    }

    def check(name: String) = {
      val files = List(s"tests/idempotency/$name.scala", "tests/idempotency/IdempotencyCheck.scala")
      compileList(name, files, defaultOptions)(TestGroup("idempotency/check"))
    }
    val allChecks = aggregateTests(
      check("CheckOrderIdempotency"),
      // Disabled until strawman is fixed
      // check("CheckStrawmanIdempotency"),
      check("CheckPosIdempotency")
    )

    val allTests = aggregateTests(orderIdempotency, posIdempotency)

    val tests = allTests.keepOutput.checkCompile()
    allChecks.checkRuns()
    tests.delete()
  }

}

object IdempotencyTests {
  implicit val summaryReport: SummaryReporting = new SummaryReport
  @AfterClass def cleanup(): Unit = summaryReport.echoSummary()
}
