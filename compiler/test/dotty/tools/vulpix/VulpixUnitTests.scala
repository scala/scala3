package dotty.tools
package vulpix

import org.junit.Assert._
import org.junit.Test

import scala.concurrent.duration._
import scala.util.control.NonFatal

/** Unit tests for the Vulpix test suite */
class VulpixUnitTests extends ParallelTesting {
  import TestConfiguration._

  implicit val _: SummaryReporting = new NoSummaryReport

  implicit def testGroup: TestGroup = TestGroup("VulpixTests")

  def maxDuration = 3.seconds
  def numberOfSlaves = 5
  def safeMode = sys.env.get("SAFEMODE").isDefined
  def isInteractive = !sys.env.contains("DRONE")
  def testFilter = None

  @Test def missingFile: Unit =
    try {
      compileFile("tests/vulpix-tests/unit/i-dont-exist.scala", defaultOptions).expectFailure.checkExpectedErrors()
      fail("didn't fail properly")
    }
    catch {
      case _: IllegalArgumentException => // pass!
      case NonFatal(_) => fail("wrong exception thrown")
    }

  @Test def pos1Error: Unit =
    compileFile("tests/vulpix-tests/unit/posFail1Error.scala", defaultOptions).expectFailure.checkCompile()

  @Test def negMissingAnnot: Unit =
    compileFile("tests/vulpix-tests/unit/negMissingAnnot.scala", defaultOptions).expectFailure.checkExpectedErrors()

  @Test def negAnnotWrongLine: Unit =
    compileFile("tests/vulpix-tests/unit/negAnnotWrongLine.scala", defaultOptions).expectFailure.checkExpectedErrors()

  @Test def negTooManyAnnots: Unit =
    compileFile("tests/vulpix-tests/unit/negTooManyAnnots.scala", defaultOptions).expectFailure.checkExpectedErrors()

  @Test def negNoPositionAnnot: Unit =
    compileFile("tests/vulpix-tests/unit/negNoPositionAnnots.scala", defaultOptions).expectFailure.checkExpectedErrors()

  @Test def runCompileFail: Unit =
    compileFile("tests/vulpix-tests/unit/posFail1Error.scala", defaultOptions).expectFailure.checkRuns()

  @Test def runWrongOutput1: Unit =
    compileFile("tests/vulpix-tests/unit/runWrongOutput1.scala", defaultOptions).expectFailure.checkRuns()

  @Test def runWrongOutput2: Unit =
    compileFile("tests/vulpix-tests/unit/runWrongOutput2.scala", defaultOptions).expectFailure.checkRuns()

  @Test def runDiffOutput1: Unit =
    compileFile("tests/vulpix-tests/unit/runDiffOutput1.scala", defaultOptions).expectFailure.checkRuns()

  @Test def runStackOverflow: Unit =
    compileFile("tests/vulpix-tests/unit/stackOverflow.scala", defaultOptions).expectFailure.checkRuns()

  @Test def runOutRedirects: Unit =
    compileFile("tests/vulpix-tests/unit/i2147.scala", defaultOptions).expectFailure.checkRuns()

  @Test def infiteNonRec: Unit =
    compileFile("tests/vulpix-tests/unit/infinite.scala", defaultOptions).expectFailure.checkRuns()

  @Test def infiteTailRec: Unit =
    compileFile("tests/vulpix-tests/unit/infiniteTail.scala", defaultOptions).expectFailure.checkRuns()

  @Test def infiniteAlloc: Unit =
    compileFile("tests/vulpix-tests/unit/infiniteAlloc.scala", defaultOptions).expectFailure.checkRuns()

  @Test def deadlock: Unit =
    compileFile("tests/vulpix-tests/unit/deadlock.scala", defaultOptions).expectFailure.checkRuns()

  @Test def badJava: Unit =
    try compileFile("tests/vulpix-tests/unit/BadJava.java", defaultOptions).suppressAllOutput.checkCompile()
    catch {
      case ae: AssertionError => assert(ae.getMessage.contains("java compilation failed"))
    }

  @Test def runTimeout: Unit = {
    try {
      compileFile("tests/vulpix-tests/unit/timeout.scala", defaultOptions).checkRuns()
      assert(false, "unreachable")
    } catch {
      case ae: AssertionError =>
        assert(ae.getMessage == "Run test failed, but should not, reasons:\n  - test 'tests/vulpix-tests/unit/timeout.scala' timed out")
    }
  }
}
