package dotty
package tools
package dotc

import org.junit.{ Test, BeforeClass, AfterClass }
import org.junit.Assert._
import org.junit.Assume._
import org.junit.experimental.categories.Category

import java.nio.file._
import java.util.stream.{ Stream => JStream }
import scala.collection.JavaConverters._
import scala.util.matching.Regex
import scala.concurrent.duration._
import TestSources.sources
import vulpix._

class CompilationTests extends ParallelTesting {
  import ParallelTesting._
  import TestConfiguration._
  import CompilationTests._
  import CompilationTest.aggregateTests

  // Test suite configuration --------------------------------------------------

  def maxDuration = 45.seconds
  def numberOfSlaves = 5
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter
  def updateCheckFiles: Boolean = Properties.testsUpdateCheckfile

  // Positive tests ------------------------------------------------------------

  // @Test  // enable to test compileStdLib separately with detailed stats
  def compileStdLibOnly: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compileStdLibOnly")
    compileList("compileStdLib", TestSources.stdLibSources, scala2Mode.and("-migration", "-Yno-inline"))
  }.checkCompile()

  @Test def pos: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compilePos")
    aggregateTests(
      compileList("compileStdLib", TestSources.stdLibSources, scala2Mode.and("-migration", "-Yno-inline")),
      compileFile("tests/pos/nullarify.scala", defaultOptions.and("-Ycheck:nullarify")),
      compileFile("tests/pos-scala2/rewrites.scala", scala2Mode.and("-rewrite")).copyToTarget(),
      compileFile("tests/pos-special/utf8encoded.scala", explicitUTF8),
      compileFile("tests/pos-special/utf16encoded.scala", explicitUTF16),
      compileFile("tests/pos-special/completeFromSource/Test.scala", defaultOptions.and("-sourcepath", "tests/pos-special")),
      compileFile("tests/pos-special/completeFromSource/Test2.scala", defaultOptions.and("-sourcepath", "tests/pos-special")),
      compileFile("tests/pos-special/completeFromSource/Test3.scala", defaultOptions.and("-sourcepath", "tests/pos-special", "-scansource")),
      compileFile("tests/pos-special/completeFromSource/nested/Test4.scala", defaultOptions.and("-sourcepath", "tests/pos-special", "-scansource")),
      compileFile("tests/pos-special/repeatedArgs213.scala", defaultOptions.and("-Ynew-collections")),
      compileFilesInDir("tests/pos-special/fatal-warnings", defaultOptions.and("-Xfatal-warnings", "-feature")),
      compileList(
        "compileMixed",
        List(
          "tests/pos/B.scala",
          "tests/scala2-library/src/library/scala/collection/immutable/Seq.scala",
          "tests/scala2-library/src/library/scala/collection/parallel/ParSeq.scala",
          "tests/scala2-library/src/library/scala/package.scala",
          "tests/scala2-library/src/library/scala/collection/GenSeqLike.scala",
          "tests/scala2-library/src/library/scala/collection/SeqLike.scala",
          "tests/scala2-library/src/library/scala/collection/generic/GenSeqFactory.scala"
        ),
        scala2Mode
      ),
      compileFilesInDir("tests/pos-special/spec-t5545", defaultOptions),
      compileFilesInDir("tests/pos-special/strawman-collections", defaultOptions),
      compileFilesInDir("tests/pos-special/isInstanceOf", allowDeepSubtypes.and("-Xfatal-warnings")),
      compileFile("tests/scala2-library/src/library/scala/collection/immutable/IndexedSeq.scala", defaultOptions),
      compileFile("tests/scala2-library/src/library/scala/collection/parallel/mutable/ParSetLike.scala", defaultOptions),
      compileList(
        "parSetSubset",
        List(
         "tests/scala2-library/src/library/scala/collection/parallel/mutable/ParSetLike.scala",
         "tests/scala2-library/src/library/scala/collection/parallel/mutable/ParSet.scala",
         "tests/scala2-library/src/library/scala/collection/mutable/SetLike.scala"
        ),
        scala2Mode
      ),
      // FIXME: This fails with .times(2), see #2799
      compileList(
        "testPredefDeprecatedNonCyclic",
        List(
          "tests/scala2-library/src/library/scala/io/Position.scala",
          "tests/scala2-library/src/library/scala/Predef.scala",
          "tests/scala2-library/src/library/scala/deprecated.scala"
        ),
        scala2Mode
      ),
      compileFilesInDir("tests/new", defaultOptions),
      compileFilesInDir("tests/pos-scala2", scala2Mode),
      compileFilesInDir("tests/pos", defaultOptions),
      compileFilesInDir("tests/pos-deep-subtype", allowDeepSubtypes),
      compileFile(
        // succeeds despite -Xfatal-warnings because of -nowarn
        "tests/neg-custom-args/fatal-warnings/xfatalWarnings.scala",
        defaultOptions.and("-nowarn", "-Xfatal-warnings")
      ),
      compileFile("tests/pos-special/typeclass-scaling.scala", defaultOptions.and("-Xmax-inlines", "40"))
    ).checkCompile()
  }

  @Test def posTwice: Unit = {
    implicit val testGroup: TestGroup = TestGroup("posTwice")
    aggregateTests(
      compileFilesInDir("tests/pos-java-interop", defaultOptions),
      compileFilesInDir("tests/pos-java-interop-separate", defaultOptions),
      compileFile("tests/pos/t2168.scala", defaultOptions),
      compileFile("tests/pos/erasure.scala", defaultOptions),
      compileFile("tests/pos/Coder.scala", defaultOptions),
      compileFile("tests/pos/blockescapes.scala", defaultOptions),
      compileFile("tests/pos/collections.scala", defaultOptions),
      compileFile("tests/pos/functions1.scala", defaultOptions),
      compileFile("tests/pos/implicits1.scala", defaultOptions),
      compileFile("tests/pos/inferred.scala", defaultOptions),
      compileFile("tests/pos/selftypes.scala", defaultOptions),
      compileFile("tests/pos/varargs.scala", defaultOptions),
      compileFile("tests/pos/vararg-pattern.scala", defaultOptions),
      compileFile("tests/pos/opassign.scala", defaultOptions),
      compileFile("tests/pos/typedapply.scala", defaultOptions),
      compileFile("tests/pos/nameddefaults.scala", defaultOptions),
      compileFile("tests/pos/desugar.scala", defaultOptions),
      compileFile("tests/pos/sigs.scala", defaultOptions),
      compileFile("tests/pos/typers.scala", defaultOptions),
      compileDir("tests/pos/typedIdents", defaultOptions),
      compileFile("tests/pos/assignments.scala", defaultOptions),
      compileFile("tests/pos/packageobject.scala", defaultOptions),
      compileFile("tests/pos/overloaded.scala", defaultOptions),
      compileFile("tests/pos/overrides.scala", defaultOptions),
      compileDir("tests/pos/java-override", defaultOptions),
      compileFile("tests/pos/templateParents.scala", defaultOptions),
      compileFile("tests/pos/overloadedAccess.scala", defaultOptions),
      compileFile("tests/pos/approximateUnion.scala", defaultOptions),
      compileFilesInDir("tests/pos/tailcall", defaultOptions),
      compileShallowFilesInDir("tests/pos/pos_valueclasses", defaultOptions),
      compileFile("tests/pos/subtyping.scala", defaultOptions),
      compileFile("tests/pos/i0239.scala", defaultOptions),
      compileFile("tests/pos/anonClassSubtyping.scala", defaultOptions),
      compileFile("tests/pos/extmethods.scala", defaultOptions),
      compileFile("tests/pos/companions.scala", defaultOptions)
    ).times(2).checkCompile()
  }

  // Negative tests ------------------------------------------------------------

  @Test def negAll: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compileNeg")
    aggregateTests(
      compileFilesInDir("tests/neg", defaultOptions),
      compileFilesInDir("tests/neg-tailcall", defaultOptions),
      compileFilesInDir("tests/neg-strict", defaultOptions.and("-strict")),
      compileFilesInDir("tests/neg-no-kind-polymorphism", defaultOptions and "-Yno-kind-polymorphism"),
      compileFilesInDir("tests/neg-custom-args/deprecation", defaultOptions.and("-Xfatal-warnings", "-deprecation")),
      compileFilesInDir("tests/neg-custom-args/fatal-warnings", defaultOptions.and("-Xfatal-warnings")),
      compileFilesInDir("tests/neg-custom-args/allow-double-bindings", allowDoubleBindings),
      compileDir("tests/neg-custom-args/impl-conv", defaultOptions.and("-Xfatal-warnings", "-feature")),
      compileFile("tests/neg-custom-args/implicit-conversions.scala", defaultOptions.and("-Xfatal-warnings", "-feature")),
      compileFile("tests/neg-custom-args/implicit-conversions-old.scala", defaultOptions.and("-Xfatal-warnings", "-feature")),
      compileFile("tests/neg-custom-args/i3246.scala", scala2Mode),
      compileFile("tests/neg-custom-args/overrideClass.scala", scala2Mode),
      compileFile("tests/neg-custom-args/ovlazy.scala", scala2Mode.and("-migration", "-Xfatal-warnings")),
      compileFile("tests/neg-custom-args/autoTuplingTest.scala", defaultOptions.and("-language:noAutoTupling")),
      compileFile("tests/neg-custom-args/nopredef.scala", defaultOptions.and("-Yno-predef")),
      compileFile("tests/neg-custom-args/noimports.scala", defaultOptions.and("-Yno-imports")),
      compileFile("tests/neg-custom-args/noimports2.scala", defaultOptions.and("-Yno-imports")),
      compileFile("tests/neg-custom-args/i1650.scala", allowDeepSubtypes),
      compileFile("tests/neg-custom-args/i3882.scala", allowDeepSubtypes),
      compileFile("tests/neg-custom-args/i4372.scala", allowDeepSubtypes),
      compileFile("tests/neg-custom-args/i1754.scala", allowDeepSubtypes),
      compileFile("tests/neg-custom-args/conditionalWarnings.scala", allowDeepSubtypes.and("-deprecation").and("-Xfatal-warnings")),
      compileFilesInDir("tests/neg-custom-args/isInstanceOf", allowDeepSubtypes and "-Xfatal-warnings"),
      compileFile("tests/neg-custom-args/i3627.scala", allowDeepSubtypes),
      compileFile("tests/neg-custom-args/matchtype-loop.scala", allowDeepSubtypes),
      compileFile("tests/neg-custom-args/completeFromSource/nested/Test1.scala", defaultOptions.and("-sourcepath", "tests/neg-custom-args", "-scansource")),
      compileFile("tests/neg-custom-args/repeatedArgs213.scala", defaultOptions.and("-Ynew-collections")),
      compileList("duplicate source", List(
        "tests/neg-custom-args/toplevel-samesource/S.scala",
        "tests/neg-custom-args/toplevel-samesource/nested/S.scala"),
        defaultOptions),
      compileFile("tests/neg-custom-args/i6300.scala", allowDeepSubtypes),
      compileFile("tests/neg-custom-args/infix.scala", defaultOptions.and("-strict", "-deprecation", "-Xfatal-warnings")),
      compileFile("tests/neg-custom-args/missing-alpha.scala", defaultOptions.and("-strict", "-deprecation", "-Xfatal-warnings")),
      compileFile("tests/neg-custom-args/wildcards.scala", defaultOptions.and("-strict", "-deprecation", "-Xfatal-warnings"))
    ).checkExpectedErrors()
  }

  @Test def fuzzyAll: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compileFuzzy")
    compileFilesInDir("tests/fuzzy", defaultOptions).checkNoCrash()
  }

  // Run tests -----------------------------------------------------------------

  @Test def runAll: Unit = {
    implicit val testGroup: TestGroup = TestGroup("runAll")
    aggregateTests(
      compileFilesInDir("tests/run-custom-args/Yretain-trees", defaultOptions and "-Yretain-trees"),
      compileFile("tests/run-custom-args/tuple-cons.scala", allowDeepSubtypes),
      compileFile("tests/run-custom-args/i5256.scala", allowDeepSubtypes),
      compileFile("tests/run-custom-args/fors.scala", defaultOptions and "-strict"),
      compileFile("tests/run-custom-args/no-useless-forwarders.scala", defaultOptions and "-Xmixin-force-forwarders:false"),
      compileFilesInDir("tests/run-deep-subtype", allowDeepSubtypes),
      compileFilesInDir("tests/run", defaultOptions)
    ).checkRuns()
  }

  // Generic java signatures tests ---------------------------------------------

  @Test def genericJavaSignatures: Unit = {
    implicit val testGroup: TestGroup = TestGroup("genericJavaSignatures")
    compileFilesInDir("tests/generic-java-signatures", defaultOptions).checkRuns()
  }

  // Pickling Tests ------------------------------------------------------------

  @Test def pickling: Unit = {
    implicit val testGroup: TestGroup = TestGroup("testPickling")
    aggregateTests(
      compileFilesInDir("tests/new", picklingOptions),
      compileFilesInDir("tests/pos", picklingOptions, FileFilter.exclude(TestSources.posTestPicklingBlacklisted)),
      compileFilesInDir("tests/run", picklingOptions, FileFilter.exclude(TestSources.runTestPicklingBlacklisted))
    ).checkCompile()
  }

  /** The purpose of this test is two-fold, being able to compile dotty
   *  bootstrapped, and making sure that TASTY can link against a compiled
   *  version of Dotty
   */
  @Test def tastyBootstrap: Unit = {
    implicit val testGroup: TestGroup = TestGroup("tastyBootstrap/tests")
    val dotty1Group = TestGroup("tastyBootstrap/dotty1")
    val dotty2Group = TestGroup("tastyBootstrap/dotty2")
    val libGroup = TestGroup("tastyBootstrap/lib")

    // Make sure that the directory is clean
    dotty.tools.io.Directory(defaultOutputDir + "tastyBootstrap").deleteRecursively()

    val sep = java.io.File.pathSeparator

    val opt = TestFlags(
      // compile with bootstrapped library on cp:
      defaultOutputDir + libGroup + "/src/" + sep +
      // as well as bootstrapped compiler:
      defaultOutputDir + dotty1Group + "/dotty/" + sep +
      // and the other compiler dependenies:
      Properties.compilerInterface + sep + Properties.scalaLibrary + sep + Properties.scalaAsm + sep +
      Properties.dottyInterfaces + sep + Properties.jlineTerminal + sep + Properties.jlineReader,
      Array("-Ycheck-reentrant", "-Yemit-tasty-in-class")
    )

    val libraryDirs = List(Paths.get("library/src"), Paths.get("library/src-3.x"), Paths.get("library/src-bootstrapped"))
    val librarySources = libraryDirs.flatMap(sources(_))

    val lib =
      compileList("src", librarySources,
        defaultOptions.and("-Ycheck-reentrant",
          //  "-strict",  // TODO: re-enable once we allow : @unchecked in pattern definitions. Right now, lots of narrowing pattern definitions fail.
          "-priorityclasspath", defaultOutputDir))(libGroup)

    val compilerSources = sources(Paths.get("compiler/src"))
    val compilerManagedSources = sources(Properties.dottyCompilerManagedSources)

    val dotty1 = compileList("dotty", compilerSources ++ compilerManagedSources, opt)(dotty1Group)
    val dotty2 = compileList("dotty", compilerSources ++ compilerManagedSources, opt)(dotty2Group)

    val tests = {
      lib.keepOutput :: dotty1.keepOutput :: aggregateTests(
        dotty2,
        compileShallowFilesInDir("compiler/src/dotty/tools", opt),
        compileShallowFilesInDir("compiler/src/dotty/tools/dotc", opt),
        compileShallowFilesInDir("compiler/src/dotty/tools/dotc/ast", opt),
        compileShallowFilesInDir("compiler/src/dotty/tools/dotc/config", opt),
        compileShallowFilesInDir("compiler/src/dotty/tools/dotc/parsing", opt),
        compileShallowFilesInDir("compiler/src/dotty/tools/dotc/printing", opt),
        compileShallowFilesInDir("compiler/src/dotty/tools/dotc/reporting", opt),
        compileShallowFilesInDir("compiler/src/dotty/tools/dotc/rewrites", opt),
        compileShallowFilesInDir("compiler/src/dotty/tools/dotc/transform", opt),
        compileShallowFilesInDir("compiler/src/dotty/tools/dotc/typer", opt),
        compileShallowFilesInDir("compiler/src/dotty/tools/dotc/util", opt),
        compileShallowFilesInDir("compiler/src/dotty/tools/backend", opt),
        compileShallowFilesInDir("compiler/src/dotty/tools/backend/jvm", opt),
        compileList("managed-sources", compilerManagedSources, opt)
      ).keepOutput :: Nil
    }.map(_.checkCompile())

    def assertExists(path: String) = assertTrue(Files.exists(Paths.get(path)))
    assertExists(s"out/$dotty1Group/dotty/")
    assertExists(s"out/$dotty2Group/dotty/")
    assertExists(s"out/$libGroup/src/")
    compileList("idempotency", List("tests/idempotency/BootstrapChecker.scala", "tests/idempotency/IdempotencyCheck.scala"), defaultOptions).checkRuns()

    tests.foreach(_.delete())
  }
}

object CompilationTests {
  implicit val summaryReport: SummaryReporting = new SummaryReport
  @AfterClass def cleanup(): Unit = summaryReport.echoSummary()
}
