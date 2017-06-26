package dotty.tools
package vulpix

import org.junit.Assert._
import org.junit.Test

import scala.concurrent.duration._
import scala.util.control.NonFatal

/** Meta tests for the Vulpix test suite */
class VulpixTests extends ParallelTesting {
  import TestConfiguration._

  implicit val _: SummaryReporting = new NoSummaryReport

  def maxDuration = 3.seconds
  def numberOfSlaves = 5
  def safeMode = sys.env.get("SAFEMODE").isDefined
  def isInteractive = !sys.env.contains("DRONE")
  def testFilter = None

  @Test def missingFile: Unit =
    try {
      compileFile("../tests/partest-test/i-dont-exist.scala", defaultOptions).expectFailure.checkExpectedErrors()
      fail("didn't fail properly")
    }
    catch {
      case _: IllegalArgumentException => // pass!
      case NonFatal(_) => fail("wrong exception thrown")
    }

  @Test def pos1Error: Unit =
    compileFile("../tests/partest-test/posFail1Error.scala", defaultOptions).expectFailure.checkCompile()

  @Test def negMissingAnnot: Unit =
    compileFile("../tests/partest-test/negMissingAnnot.scala", defaultOptions).expectFailure.checkExpectedErrors()

  @Test def negAnnotWrongLine: Unit =
    compileFile("../tests/partest-test/negAnnotWrongLine.scala", defaultOptions).expectFailure.checkExpectedErrors()

  @Test def negTooManyAnnots: Unit =
    compileFile("../tests/partest-test/negTooManyAnnots.scala", defaultOptions).expectFailure.checkExpectedErrors()

  @Test def negNoPositionAnnot: Unit =
    compileFile("../tests/partest-test/negNoPositionAnnots.scala", defaultOptions).expectFailure.checkExpectedErrors()

  @Test def runCompileFail: Unit =
    compileFile("../tests/partest-test/posFail1Error.scala", defaultOptions).expectFailure.checkRuns()

  @Test def runWrongOutput1: Unit =
    compileFile("../tests/partest-test/runWrongOutput1.scala", defaultOptions).expectFailure.checkRuns()

  @Test def runWrongOutput2: Unit =
    compileFile("../tests/partest-test/runWrongOutput2.scala", defaultOptions).expectFailure.checkRuns()

  @Test def runDiffOutput1: Unit =
    compileFile("../tests/partest-test/runDiffOutput1.scala", defaultOptions).expectFailure.checkRuns()

  @Test def runStackOverflow: Unit =
    compileFile("../tests/partest-test/stackOverflow.scala", defaultOptions).expectFailure.checkRuns()

  @Test def runOutRedirects: Unit =
    compileFile("../tests/partest-test/i2147.scala", defaultOptions).expectFailure.checkRuns()

  @Test def infiteNonRec: Unit =
    compileFile("../tests/partest-test/infinite.scala", defaultOptions).expectFailure.checkRuns()

  @Test def infiteTailRec: Unit =
    compileFile("../tests/partest-test/infiniteTail.scala", defaultOptions).expectFailure.checkRuns()

  @Test def infiniteAlloc: Unit =
    compileFile("../tests/partest-test/infiniteAlloc.scala", defaultOptions).expectFailure.checkRuns()

  @Test def deadlock: Unit =
    compileFile("../tests/partest-test/deadlock.scala", defaultOptions).expectFailure.checkRuns()

  @Test def badJava: Unit =
    try compileFile("../tests/partest-test/BadJava.java", defaultOptions).suppressAllOutput.checkCompile
    catch {
      case ae: AssertionError => assert(ae.getMessage.contains("java compilation failed"))
    }
}
