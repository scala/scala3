package xsbt

import org.junit.{ Test, Ignore }
import org.junit.Assert._

/**Only does some rudimentary checks to assert compat with sbt.
 * More thorough tests are found in compiler/test/dotty/tools/dotc/sbt/ProgressCallbackTest.scala
 */
class CompileProgressSpecification {

  @Test
  def multipleFilesVisitSamePhases = {
    val srcA = """class A"""
    val srcB = """class B"""
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val Seq(phasesA, phasesB) = compilerForTesting.extractEnteredPhases(srcA, srcB)
    assertTrue("expected some phases, was empty", phasesA.nonEmpty)
    assertEquals(phasesA, phasesB)
  }

  @Test
  def multipleFiles = {
    val srcA = """class A"""
    val srcB = """class B"""
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val allPhases = compilerForTesting.extractProgressPhases(srcA, srcB)
    assertTrue("expected some phases, was empty", allPhases.nonEmpty)
    val someExpectedPhases = // just check some "fundamental" phases, don't put all phases to avoid brittleness
      Set(
        "parser",
        "typer (indexing)", "typer (typechecking)", "typer (checking java)",
        "sbt-deps",
        "extractSemanticDB",
        "posttyper",
        "sbt-api",
        "SetRootTree",
        "pickler",
        "inlining",
        "postInlining",
        "staging",
        "splicing",
        "pickleQuotes",
        "MegaPhase{pruneErasedDefs,...,arrayConstructors}",
        "erasure",
        "constructors",
        "genSJSIR",
        "genBCode"
      )
    val missingExpectedPhases = someExpectedPhases -- allPhases.toSet
    val msgIfMissing =
      s"missing expected phases: $missingExpectedPhases. " +
      s"Either the compiler phases changed, or the encoding of Run.SubPhases.subPhase"
    assertTrue(msgIfMissing, missingExpectedPhases.isEmpty)
  }

}
