package dotty
package tools
package dotc

import java.io.{File => JFile}
import java.nio.file.{Files, Path, Paths}

import org.junit.Assume.assumeTrue
import org.junit.{AfterClass, Test}
import org.junit.experimental.categories.Category

import scala.concurrent.duration._
import vulpix.{ParallelTesting, SummaryReport, SummaryReporting, TestConfiguration}


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

    val opt = defaultOptions.and("-YemitTasty")

    def sourcesFrom(dir: Path) = CompilationTests.sources(Files.walk(dir))

    val strawmanSources = sourcesFrom(Paths.get("../collection-strawman/src/main"))
    val strawmanSourcesSorted = strawmanSources.sorted
    val strawmanSourcesRevSorted = strawmanSourcesSorted.reverse

    val posIdempotency = {
      def posIdempotency1 = compileFilesInDir("../tests/pos", opt)
      def posIdempotency2 = compileFilesInDir("../tests/pos", opt)
      posIdempotency1 + posIdempotency2
    }

    val orderIdempotency = {
      (for {
        testDir <- new JFile("../tests/order-idempotency").listFiles() if testDir.isDirectory
      } yield {
        val sources = sourcesFrom(testDir.toPath)
        def orderIdempotency1 = compileList(testDir.getName, sources, opt)
        def orderIdempotency2 = compileList(testDir.getName, sources.reverse, opt)
        orderIdempotency1 + orderIdempotency2
      }).reduce(_ + _)
    }

    val strawmanIdempotency = {
      compileList("strawman0", strawmanSources, opt) +
      compileList("strawman1", strawmanSources, opt) +
      compileList("strawman2", strawmanSourcesSorted, opt) +
      compileList("strawman3", strawmanSourcesRevSorted, opt)
    }

    def check(name: String) = {
      val files = List(s"../tests/idempotency/$name.scala", "../tests/idempotency/IdempotencyCheck.scala")
      compileList(name, files, defaultOptions)
    }
    val allChecks = {
      check("CheckOrderIdempotency") +
      check("CheckStrawmanIdempotency") +
      check("CheckPosIdempotency")
    }

    val allTests = {
      strawmanIdempotency +
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
