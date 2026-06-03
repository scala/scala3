package dotty.tools
package vulpix

import org.junit.{Test as test, AfterClass as tearDown}

/** Unit tests for the Vulpix test suite */
class VulpixUnitTests:
  import VulpixUnitTests.*
  import TestConfiguration.*

  given SummaryReporting = new NoSummaryReport

  given TestGroup = TestGroup("VulpixTests")

  @test def missingFile: Unit =
    assertThrows[IllegalArgumentException](_ => true):
      compileFile("tests/vulpix-tests/unit/i-dont-exist.scala", defaultOptions).expectFailure.checkExpectedErrors()

  @test def pos1Error: Unit =
    compileFile("tests/vulpix-tests/unit/posFail1Error.scala", defaultOptions).expectFailure.checkCompile()

  @test def negMissingAnnot: Unit =
    compileFile("tests/vulpix-tests/unit/negMissingAnnot.scala", defaultOptions)
      .suppressAllOutput
      .expectFailure
      .checkExpectedErrors()

  @test def negAnnotWrongLine: Unit =
    compileFile("tests/vulpix-tests/unit/negAnnotWrongLine.scala", defaultOptions)
      .suppressAllOutput
      .expectFailure
      .checkExpectedErrors()

  @test def negTooManyAnnots: Unit =
    compileFile("tests/vulpix-tests/unit/negTooManyAnnots.scala", defaultOptions)
      .suppressAllOutput
      .expectFailure
      .checkExpectedErrors()

  @test def negNoPositionAnnot: Unit =
    compileFile("tests/vulpix-tests/unit/negNoPositionAnnots.scala", defaultOptions)
      .suppressAllOutput
      .expectFailure
      .checkExpectedErrors()

  @test def negAnyPositionAnnot: Unit =
    compileFile("tests/vulpix-tests/unit/negAnyPositionAnnots.scala", defaultOptions)
      .suppressAllOutput
      .checkExpectedErrors()

  @test def runCompileFail: Unit =
    compileFile("tests/vulpix-tests/unit/posFail1Error.scala", defaultOptions).expectFailure.checkRuns()

  @test def runWrongOutput1: Unit =
    compileFile("tests/vulpix-tests/unit/runWrongOutput1.scala", defaultOptions).expectFailure.checkRuns()

  @test def runWrongOutput2: Unit =
    compileFile("tests/vulpix-tests/unit/runWrongOutput2.scala", defaultOptions).expectFailure.checkRuns()

  @test def runDiffOutput1: Unit =
    compileFile("tests/vulpix-tests/unit/runDiffOutput1.scala", defaultOptions).expectFailure.checkRuns()

  @test def runStackOverflow: Unit =
    compileFile("tests/vulpix-tests/unit/stackOverflow.scala", defaultOptions).expectFailure.checkRuns()

  @test def runOutRedirects: Unit =
    compileFile("tests/vulpix-tests/unit/i2147.scala", defaultOptions).expectFailure.checkRuns()

  @test def infiniteNonRec: Unit =
    compileFile("tests/vulpix-tests/unit/infinite.scala", defaultOptions).expectFailure.checkRuns()

  @test def infiniteTailRec: Unit =
    compileFile("tests/vulpix-tests/unit/infiniteTail.scala", defaultOptions).expectFailure.checkRuns()

  @test def infiniteAlloc: Unit =
    compileFile("tests/vulpix-tests/unit/infiniteAlloc.scala", defaultOptions).expectFailure.checkRuns()

  @test def deadlock: Unit =
    compileFile("tests/vulpix-tests/unit/deadlock.scala", defaultOptions).expectFailure.checkRuns()

  @test def badJava: Unit =
    assertThrows[AssertionError](_.getMessage.contains("java compilation failed")):
      compileFile("tests/vulpix-tests/unit/BadJava.java", defaultOptions)
        .suppressAllOutput
        .checkCompile()

  @test def runTimeout: Unit =
    val fileName = s"tests/vulpix-tests/unit/timeout.scala"
    val expect = """(?m).*test '.+' timed out.*"""
    assertThrows[AssertionError](_.getMessage.linesIterator.toList.last.matches(expect)):
      compileFile(fileName, defaultOptions)
        .suppressAllOutput
        .checkRuns()

object VulpixUnitTests extends ParallelTesting:
  import scala.concurrent.duration.*
  def maxDuration = 3.seconds
  def numberOfWorkers = 5
  def safeMode = sys.env.get("SAFEMODE").isDefined
  def isInteractive = !sys.env.contains("DOTTY_CI_RUN")
  def testFilter = Nil
  def updateCheckFiles: Boolean = false
  def failedTests = None

  @tearDown
  def tearDown() = cleanup()
