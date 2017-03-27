package dotty
package tools
package dotc

import org.junit.Assert._
import org.junit.Test

import scala.util.control.NonFatal

class ParallelTestTests extends ParallelTesting {
  import CompilationTests._

  def interactive: Boolean = !sys.env.contains("DRONE")

  def regex: Option[String] = None

  @Test def missingFile: Unit =
    try {
      compileFile("../tests/partest-test/i-dont-exist.scala", defaultOptions).expectFailure.neg()
      fail("didn't fail properly")
    }
    catch {
      case _: IllegalArgumentException => // pass!
      case NonFatal(_) => fail("wrong exception thrown")
    }

  @Test def pos1Error: Unit =
    compileFile("../tests/partest-test/posFail1Error.scala", defaultOptions).expectFailure.pos()

  @Test def negMissingAnnot: Unit =
    compileFile("../tests/partest-test/negMissingAnnot.scala", defaultOptions).expectFailure.neg()

  @Test def negAnnotWrongLine: Unit =
    compileFile("../tests/partest-test/negAnnotWrongLine.scala", defaultOptions).expectFailure.neg()

  @Test def negTooManyAnnots: Unit =
    compileFile("../tests/partest-test/negTooManyAnnots.scala", defaultOptions).expectFailure.neg()

  @Test def negNoPositionAnnot: Unit =
    compileFile("../tests/partest-test/negNoPositionAnnots.scala", defaultOptions).expectFailure.neg()

  @Test def runCompileFail: Unit =
    compileFile("../tests/partest-test/posFail1Error.scala", defaultOptions).expectFailure.run()

  @Test def runWrongOutput1: Unit =
    compileFile("../tests/partest-test/runWrongOutput1.scala", defaultOptions).expectFailure.run()

  @Test def runWrongOutput2: Unit =
    compileFile("../tests/partest-test/runWrongOutput2.scala", defaultOptions).expectFailure.run()

  @Test def runDiffOutput1: Unit =
    compileFile("../tests/partest-test/runDiffOutput1.scala", defaultOptions).expectFailure.run()
}
