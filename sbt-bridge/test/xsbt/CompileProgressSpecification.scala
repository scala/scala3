package xsbt

import org.junit.{ Test, Ignore }
import org.junit.Assert._

/**Only does some rudimentary checks to assert compat with sbt.
 * More thorough tests are found in compiler/test/dotty/tools/dotc/sbt/ProgressCallbackTest.scala
 */
class CompileProgressSpecification {

  @Test
  def totalIsMoreWhenSourcePath = {
    val srcA = """class A"""
    val srcB = """class B"""
    val extraC = """trait C""" // will only exist in the `-sourcepath`, causing a late compile
    val extraD = """trait D""" // will only exist in the `-sourcepath`, causing a late compile
    val srcE = """class E extends C""" // depends on class in the sourcepath
    val srcF = """class F extends C, D""" // depends on classes in the sourcepath

    val compilerForTesting = new ScalaCompilerForUnitTesting

    val totalA = compilerForTesting.extractTotal(srcA)()
    assertTrue("expected more than 1 unit of work for a single file", totalA > 1)

    val totalB = compilerForTesting.extractTotal(srcA, srcB)()
    assertEquals("expected twice the work for two sources", totalA * 2, totalB)

    val totalC = compilerForTesting.extractTotal(srcA, srcE)(extraC)
    assertEquals("expected 2x+1 the work for two sources, and 1 late compile", totalA * 2 + 1, totalC)

    val totalD = compilerForTesting.extractTotal(srcA, srcF)(extraC, extraD)
    assertEquals("expected 2x+2 the work for two sources, and 2 late compiles", totalA * 2 + 2, totalD)
  }

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
