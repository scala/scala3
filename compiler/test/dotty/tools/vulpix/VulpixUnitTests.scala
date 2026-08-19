package dotty.tools
package vulpix

import org.junit.Test

/** Unit tests for the Vulpix test suite */
class VulpixUnitTests:
  import VulpixUnitTests.{*, given}
  import TestConfiguration.*

  given TestGroup = TestGroup("VulpixTests")

  @Test def missingFile: Unit =
    assertThrows[IllegalArgumentException](_ => true):
      compileFile("tests/vulpix-tests/unit/i-dont-exist.scala", defaultOptions).expectFailure.checkExpectedErrors()

  @Test def pos1Error: Unit =
    compileFile("tests/vulpix-tests/unit/posFail1Error.scala", defaultOptions).expectFailure.checkCompile()

  @Test def negMissingAnnot: Unit =
    compileFile("tests/vulpix-tests/unit/negMissingAnnot.scala", defaultOptions)
      .suppressAllOutput
      .expectFailure
      .checkExpectedErrors()

  @Test def negAnnotWrongLine: Unit =
    compileFile("tests/vulpix-tests/unit/negAnnotWrongLine.scala", defaultOptions)
      .suppressAllOutput
      .expectFailure
      .checkExpectedErrors()

  @Test def negTooManyAnnots: Unit =
    compileFile("tests/vulpix-tests/unit/negTooManyAnnots.scala", defaultOptions)
      .suppressAllOutput
      .expectFailure
      .checkExpectedErrors()

  @Test def negNoPositionAnnot: Unit =
    compileFile("tests/vulpix-tests/unit/negNoPositionAnnots.scala", defaultOptions)
      .suppressAllOutput
      .expectFailure
      .checkExpectedErrors()

  @Test def negAnyPositionAnnot: Unit =
    compileFile("tests/vulpix-tests/unit/negAnyPositionAnnots.scala", defaultOptions)
      .suppressAllOutput
      .checkExpectedErrors()

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

  @Test def infiniteNonRec: Unit =
    compileFile("tests/vulpix-tests/unit/infinite.scala", defaultOptions).expectFailure.checkRuns()

  @Test def infiniteTailRec: Unit =
    compileFile("tests/vulpix-tests/unit/infiniteTail.scala", defaultOptions).expectFailure.checkRuns()

  @Test def infiniteAlloc: Unit =
    compileFile("tests/vulpix-tests/unit/infiniteAlloc.scala", defaultOptions).expectFailure.checkRuns()

  @Test def deadlock: Unit =
    compileFile("tests/vulpix-tests/unit/deadlock.scala", defaultOptions).expectFailure.checkRuns()

  @Test def badJava: Unit =
    assertThrows[AssertionError](_.getMessage.contains("java compilation failed")):
      compileFile("tests/vulpix-tests/unit/BadJava.java", defaultOptions)
        .suppressAllOutput
        .checkCompile()

  @Test def runTimeout: Unit =
    val fileName = s"tests/vulpix-tests/unit/timeout.scala"
    val expect = """(?m).*test '.+' timed out.*"""
    assertThrows[AssertionError](_.getMessage.linesIterator.toList.last.matches(expect)):
      compileFile(fileName, defaultOptions)
        .suppressAllOutput
        .checkRuns()

object VulpixUnitTests extends ParallelTesting
