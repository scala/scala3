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

  // Test suite configuration --------------------------------------------------

  def maxDuration = 30.seconds
  def numberOfSlaves = 5
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter

  @Category(Array(classOf[SlowTests]))
  @Test def idempotency: Unit = {
    implicit val testGroup: TestGroup = TestGroup("idempotency")
    val opt = defaultOptions

    def sourcesFrom(dir: Path) = CompilationTests.sources(Files.walk(dir))

    val strawmanSources = sourcesFrom(Paths.get("collection-strawman/collections/src/main"))
    val strawmanSourcesSorted = strawmanSources.sorted
    val strawmanSourcesRevSorted = strawmanSourcesSorted.reverse

    val posIdempotency = {
      compileFilesInDir("tests/pos", opt)(TestGroup("idempotency/posIdempotency1")) +
      compileFilesInDir("tests/pos", opt)(TestGroup("idempotency/posIdempotency2"))
    }

    val orderIdempotency = {
      (for {
        testDir <- new JFile("tests/order-idempotency").listFiles() if testDir.isDirectory
      } yield {
        val sources = sourcesFrom(testDir.toPath)
        compileList(testDir.getName, sources, opt)(TestGroup("idempotency/orderIdempotency1")) +
        compileList(testDir.getName, sources.reverse, opt)(TestGroup("idempotency/orderIdempotency2"))
      }).reduce(_ + _)
    }

    val strawmanIdempotency = {
      compileList("strawman0", strawmanSources, opt) +
      compileList("strawman1", strawmanSources, opt) +
      compileList("strawman2", strawmanSourcesSorted, opt) +
      compileList("strawman3", strawmanSourcesRevSorted, opt)
    }

    def check(name: String) = {
      val files = List(s"tests/idempotency/$name.scala", "tests/idempotency/IdempotencyCheck.scala")
      compileList(name, files, defaultOptions)(TestGroup("idempotency/check"))
    }
    val allChecks = {
      check("CheckOrderIdempotency") +
      // Disabled until strawman is fixed
      // check("CheckStrawmanIdempotency") +
      check("CheckPosIdempotency")
    }

    val allTests = {
      // Disabled until strawman is fixed
      // strawmanIdempotency +
      orderIdempotency +
      posIdempotency
    }

    val tests = allTests.keepOutput.checkCompile()
    allChecks.checkRuns()
    tests.delete()
  }

}

object IdempotencyTests {
  implicit val summaryReport: SummaryReporting = new SummaryReport
  @AfterClass def cleanup(): Unit = summaryReport.echoSummary()
}
