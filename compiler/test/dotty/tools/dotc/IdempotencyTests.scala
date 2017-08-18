package dotty
package tools
package dotc

import org.junit.{ Test, BeforeClass, AfterClass }

import java.nio.file._
import java.util.stream.{ Stream => JStream }
import scala.collection.JavaConverters._
import scala.util.matching.Regex
import scala.concurrent.duration._

import vulpix.{ ParallelTesting, SummaryReport, SummaryReporting, TestConfiguration }


class IdempotencyTests extends ParallelTesting {
  import TestConfiguration._
  import IdempotencyTests._

  // Test suite configuration --------------------------------------------------

  def maxDuration = 30.seconds
  def numberOfSlaves = 5
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter

  /* TODO: Only run them selectively? */
  @Test def bytecodeIdempotency: Unit = {
    val opt = defaultOptions.and("-YemitTasty")

    def idempotency1() = {
      compileDir("../collection-strawman/src/main", opt) +
      compileFilesInDir("../tests/pos", opt)
    }
    def idempotency2() = {
      compileDir("../collection-strawman/src/main", opt) +
      compileFilesInDir("../tests/pos", opt)
    }

    val tests = (idempotency1() + idempotency2()).keepOutput.checkCompile()

    assert(new java.io.File("../out/idempotency1/").exists)
    assert(new java.io.File("../out/idempotency2/").exists)

    compileList("idempotency", List("../tests/idempotency/Checker.scala", "../tests/idempotency/IdempotencyCheck.scala"), defaultOptions).checkRuns()

    tests.delete()
  }

}

object IdempotencyTests {
  implicit val summaryReport: SummaryReporting = new SummaryReport
  @AfterClass def cleanup(): Unit = summaryReport.echoSummary()
}
