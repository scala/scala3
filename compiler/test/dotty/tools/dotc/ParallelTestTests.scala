package dotty
package tools
package dotc

import org.junit.Assert._
import org.junit.Test

import scala.util.control.NonFatal

class ParallelTestTests extends ParallelTesting {
  import CompilationTests._

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

  @Test def negMissingAnnot: Unit = compileFile("../tests/partest-test/negMissingAnnot.scala", defaultOptions).expectFailure.checkExpectedErrors()

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
}
