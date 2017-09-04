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


class CompilationTests extends ParallelTesting {
  import TestConfiguration._
  import CompilationTests._

  // Test suite configuration --------------------------------------------------

  def maxDuration = 30.seconds
  def numberOfSlaves = 5
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter

  // Positive tests ------------------------------------------------------------

  @Test def compilePos: Unit = {
    compileList("compileStdLib", StdLibSources.whitelisted, scala2Mode.and("-migration", "-Yno-inline")) +
    compileDir("../collection-strawman/src/main", defaultOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/ast", defaultOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/config", defaultOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/core", allowDeepSubtypes) +
    compileDir("../compiler/src/dotty/tools/dotc/transform", allowDeepSubtypes) +
    compileDir("../compiler/src/dotty/tools/dotc/parsing", defaultOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/printing", defaultOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/reporting", defaultOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/typer", defaultOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/util", defaultOptions) +
    compileDir("../compiler/src/dotty/tools/io", defaultOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/core", noCheckOptions ++ classPath) +
    compileFile("../tests/pos/nullarify.scala", defaultOptions.and("-Ycheck:nullarify")) +
    compileFile("../tests/pos-scala2/rewrites.scala", scala2Mode.and("-rewrite")).copyToTarget() +
    compileFile("../tests/pos-special/t8146a.scala", allowDeepSubtypes) +
    compileFile("../tests/pos-special/utf8encoded.scala", explicitUTF8) +
    compileFile("../tests/pos-special/utf16encoded.scala", explicitUTF16) +
    compileList(
      "compileMixed",
      List(
        "../tests/pos/B.scala",
        "../scala2-library/src/library/scala/collection/immutable/Seq.scala",
        "../scala2-library/src/library/scala/collection/parallel/ParSeq.scala",
        "../scala2-library/src/library/scala/package.scala",
        "../scala2-library/src/library/scala/collection/GenSeqLike.scala",
        "../scala2-library/src/library/scala/collection/SeqLike.scala",
        "../scala2-library/src/library/scala/collection/generic/GenSeqFactory.scala"
      ),
      scala2Mode
    ) +
    compileFilesInDir("../tests/pos-special/spec-t5545", defaultOptions) +
    compileFilesInDir("../tests/pos-special/strawman-collections", defaultOptions) +
    compileFile("../scala2-library/src/library/scala/collection/immutable/IndexedSeq.scala", defaultOptions) +
    compileFile("../scala2-library/src/library/scala/collection/parallel/mutable/ParSetLike.scala", defaultOptions) +
    compileFile("../tests/pos/t2171.scala", defaultOptimised) +
    compileList(
      "parSetSubset",
      List(
       "../scala2-library/src/library/scala/collection/parallel/mutable/ParSetLike.scala",
       "../scala2-library/src/library/scala/collection/parallel/mutable/ParSet.scala",
       "../scala2-library/src/library/scala/collection/mutable/SetLike.scala"
      ),
      scala2Mode
    ) +
    // FIXME: This fails with .times(2), see #2799
    compileList(
      "testPredefDeprecatedNonCyclic",
      List(
        "../scala2-library/src/library/scala/io/Position.scala",
        "../scala2-library/src/library/scala/Predef.scala",
        "../scala2-library/src/library/scala/deprecated.scala"
      ),
      scala2Mode
    ) +
    compileFilesInDir("../tests/new", defaultOptions) +
    compileFilesInDir("../tests/pos-scala2", scala2Mode) +
    compileFilesInDir("../tests/pos", defaultOptions) +
    compileFile(
      // succeeds despite -Xfatal-warnings because of -nowarn
      "../tests/neg/customArgs/xfatalWarnings.scala",
      defaultOptions.and("-nowarn", "-Xfatal-warnings")
    )
  }.checkCompile()

  @Test def posTwice: Unit = {
    compileFile("../tests/pos/Labels.scala", defaultOptions) +
    compileFilesInDir("../tests/pos-java-interop", defaultOptions) +
    compileFile("../tests/pos/t2168.scala", defaultOptions) +
    compileFile("../tests/pos/erasure.scala", defaultOptions) +
    compileFile("../tests/pos/Coder.scala", defaultOptions) +
    compileFile("../tests/pos/blockescapes.scala", defaultOptions) +
    compileFile("../tests/pos/collections.scala", defaultOptions) +
    compileFile("../tests/pos/functions1.scala", defaultOptions) +
    compileFile("../tests/pos/implicits1.scala", defaultOptions) +
    compileFile("../tests/pos/inferred.scala", defaultOptions) +
    compileFile("../tests/pos/Patterns.scala", defaultOptions) +
    compileFile("../tests/pos/selftypes.scala", defaultOptions) +
    compileFile("../tests/pos/varargs.scala", defaultOptions) +
    compileFile("../tests/pos/vararg-pattern.scala", defaultOptions) +
    compileFile("../tests/pos/opassign.scala", defaultOptions) +
    compileFile("../tests/pos/typedapply.scala", defaultOptions) +
    compileFile("../tests/pos/nameddefaults.scala", defaultOptions) +
    compileFile("../tests/pos/desugar.scala", defaultOptions) +
    compileFile("../tests/pos/sigs.scala", defaultOptions) +
    compileFile("../tests/pos/typers.scala", defaultOptions) +
    compileDir("../tests/pos/typedIdents", defaultOptions) +
    compileFile("../tests/pos/assignments.scala", defaultOptions) +
    compileFile("../tests/pos/packageobject.scala", defaultOptions) +
    compileFile("../tests/pos/overloaded.scala", defaultOptions) +
    compileFile("../tests/pos/overrides.scala", defaultOptions) +
    compileDir("../tests/pos/java-override", defaultOptions) +
    compileFile("../tests/pos/templateParents.scala", defaultOptions) +
    compileFile("../tests/pos/overloadedAccess.scala", defaultOptions) +
    compileFile("../tests/pos/approximateUnion.scala", defaultOptions) +
    compileFilesInDir("../tests/pos/tailcall", defaultOptions) +
    compileShallowFilesInDir("../tests/pos/pos_valueclasses", defaultOptions) +
    compileFile("../tests/pos/subtyping.scala", defaultOptions) +
    compileFile("../tests/pos/i0239.scala", defaultOptions) +
    compileFile("../tests/pos/anonClassSubtyping.scala", defaultOptions) +
    compileFile("../tests/pos/extmethods.scala", defaultOptions) +
    compileFile("../tests/pos/companions.scala", defaultOptions) +
    compileList(
      "testNonCyclic",
      List(
        "../compiler/src/dotty/tools/dotc/CompilationUnit.scala",
        "../compiler/src/dotty/tools/dotc/core/Types.scala",
        "../compiler/src/dotty/tools/dotc/ast/Trees.scala"
      ),
      defaultOptions.and("-Xprompt")
    ) +
    compileList(
      "testIssue34",
      List(
        "../compiler/src/dotty/tools/dotc/config/Properties.scala",
        "../compiler/src/dotty/tools/dotc/config/PathResolver.scala"
      ),
      defaultOptions.and("-Xprompt")
    )
  }.times(2).checkCompile()

  // Negative tests ------------------------------------------------------------

  @Test def compileNeg: Unit = {
    compileShallowFilesInDir("../tests/neg", defaultOptions) +
    compileFile("../tests/neg/customArgs/typers.scala", allowDoubleBindings) +
    compileFile("../tests/neg/customArgs/overrideClass.scala", scala2Mode) +
    compileFile("../tests/neg/customArgs/autoTuplingTest.scala", defaultOptions.and("-language:noAutoTupling")) +
    compileFile("../tests/neg/customArgs/i1050.scala", defaultOptions.and("-strict")) +
    compileFile("../tests/neg/customArgs/i1240.scala", allowDoubleBindings) +
    compileFile("../tests/neg/customArgs/i2002.scala", allowDoubleBindings) +
    compileFile("../tests/neg/customArgs/nopredef.scala", defaultOptions.and("-Yno-predef")) +
    compileFile("../tests/neg/customArgs/noimports.scala", defaultOptions.and("-Yno-imports")) +
    compileFile("../tests/neg/customArgs/noimports2.scala", defaultOptions.and("-Yno-imports")) +
    compileFile("../tests/neg/customArgs/overloadsOnAbstractTypes.scala", allowDoubleBindings) +
    compileFile("../tests/neg/customArgs/xfatalWarnings.scala", defaultOptions.and("-Xfatal-warnings")) +
    compileFile("../tests/neg/customArgs/pureStatement.scala", defaultOptions.and("-Xfatal-warnings")) +
    compileFile("../tests/neg/customArgs/phantom-overload.scala", allowDoubleBindings) +
    compileFile("../tests/neg/customArgs/phantom-overload-2.scala", allowDoubleBindings) +
    compileFile("../tests/neg/tailcall/t1672b.scala", defaultOptions) +
    compileFile("../tests/neg/tailcall/t3275.scala", defaultOptions) +
    compileFile("../tests/neg/tailcall/t6574.scala", defaultOptions) +
    compileFile("../tests/neg/tailcall/tailrec.scala", defaultOptions) +
    compileFile("../tests/neg/tailcall/tailrec-2.scala", defaultOptions) +
    compileFile("../tests/neg/tailcall/tailrec-3.scala", defaultOptions) +
    compileDir("../tests/neg/typedIdents", defaultOptions)
  }.checkExpectedErrors()

  // Run tests -----------------------------------------------------------------

  @Test def runAll: Unit = {
    compileFilesInDir("../tests/run", defaultOptions) +
    compileFile("../tests/run/i3018.scala", defaultOptimised) +
    compileFile("../tests/run/blame_eye_triple_eee-double.scala", defaultOptimised) +
    compileFile("../tests/run/blame_eye_triple_eee-float.scala", defaultOptimised) +
    compileFile("../tests/run/run-bug4840.scala", defaultOptimised) +
    compileFile("../tests/run/optimizer-array-load.scala", defaultOptimised) +
    compileFile("../tests/run/constant-optimization.scala", defaultOptimised)
  }.checkRuns()

  // Pickling Tests ------------------------------------------------------------
  //
  // Pickling tests are very memory intensive and as such need to be run with a
  // lower level of concurrency as to not kill their running VMs

  @Test def testPickling: Unit = {
    compileDir("../compiler/src/dotty/tools", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc", picklingOptions) +
    compileFilesInDir("../tests/new", picklingOptions) +
    compileFilesInDir("../tests/pickling", picklingOptions) +
    compileDir("../library/src/dotty/runtime", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/backend/jvm", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/ast", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/core", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/config", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/parsing", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/printing", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/repl", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/rewrite", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/transform", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/typer", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/util", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/io", picklingOptions) +
    compileFile("../tests/pos/pickleinf.scala", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/core/classfile", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/core/tasty", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/core/unpickleScala2", picklingOptions)
  }.limitThreads(4).checkCompile()

  /** The purpose of this test is two-fold, being able to compile dotty
   *  bootstrapped, and making sure that TASTY can link against a compiled
   *  version of Dotty
   */
  @Test def tastyBootstrap: Unit = {
    val opt = Array(
      "-classpath",
      // compile with bootstrapped library on cp:
      defaultOutputDir + "lib/src/:" +
      // as well as bootstrapped compiler:
      defaultOutputDir + "dotty1/dotty/:" +
      Jars.dottyInterfaces,
      "-Ycheck-reentrant"
    )

    def lib =
      compileDir("../library/src",
        allowDeepSubtypes.and("-Ycheck-reentrant", "-strict", "-priorityclasspath", defaultOutputDir))

    val compilerDir = Paths.get("../compiler/src")
    val compilerSources = sources(Files.walk(compilerDir))

    val backendDir = Paths.get("../scala-backend/src/compiler/scala/tools/nsc/backend")
    val backendJvmDir = Paths.get("../scala-backend/src/compiler/scala/tools/nsc/backend/jvm")

    // NOTE: Keep these exclusions synchronized with the ones in the sbt build (Build.scala)
    val backendExcluded =
      List("JavaPlatform.scala", "Platform.scala", "ScalaPrimitives.scala")
    val backendJvmExcluded =
      List("BCodeICodeCommon.scala", "GenASM.scala", "GenBCode.scala", "ScalacBackendInterface.scala", "BackendStats.scala", "BCodeAsmEncode.scala")

    val backendSources =
      sources(Files.list(backendDir), excludedFiles = backendExcluded)
    val backendJvmSources =
      sources(Files.list(backendJvmDir), excludedFiles = backendJvmExcluded)

    def dotty1 = {
      compileList(
        "dotty",
        compilerSources ++ backendSources ++ backendJvmSources,
        opt)
    }

    def dotty2 = {
      compileList(
        "dotty",
        compilerSources ++ backendSources ++ backendJvmSources,
        opt)
    }

    val tests = {
      lib.keepOutput :: dotty1.keepOutput :: {
        dotty2 +
        compileShallowFilesInDir("../compiler/src/dotty/tools", opt) +
        compileShallowFilesInDir("../compiler/src/dotty/tools/dotc", opt) +
        compileShallowFilesInDir("../compiler/src/dotty/tools/dotc/ast", opt) +
        compileShallowFilesInDir("../compiler/src/dotty/tools/dotc/config", opt) +
        compileShallowFilesInDir("../compiler/src/dotty/tools/dotc/parsing", opt) +
        compileShallowFilesInDir("../compiler/src/dotty/tools/dotc/printing", opt) +
        compileShallowFilesInDir("../compiler/src/dotty/tools/dotc/reporting", opt) +
        compileShallowFilesInDir("../compiler/src/dotty/tools/dotc/rewrite", opt) +
        compileShallowFilesInDir("../compiler/src/dotty/tools/dotc/transform", opt) +
        compileShallowFilesInDir("../compiler/src/dotty/tools/dotc/typer", opt) +
        compileShallowFilesInDir("../compiler/src/dotty/tools/dotc/util", opt) +
        compileList("shallow-backend", backendSources, opt) +
        compileList("shallow-backend-jvm", backendJvmSources, opt)
      }.keepOutput :: Nil
    }.map(_.checkCompile())

    assert(new java.io.File("../out/dotty1/dotty/").exists)
    assert(new java.io.File("../out/dotty2/dotty/").exists)
    compileList("idempotency", List("../tests/idempotency/BootstrapChecker.scala", "../tests/idempotency/IdempotencyCheck.scala"), defaultOptions).checkRuns()

    tests.foreach(_.delete())
  }
}

object CompilationTests {
  implicit val summaryReport: SummaryReporting = new SummaryReport
  @AfterClass def cleanup(): Unit = summaryReport.echoSummary()

  def sources(paths: JStream[Path], excludedFiles: List[String] = Nil): List[String] =
    paths.iterator().asScala
      .filter(path =>
        (path.toString.endsWith(".scala") || path.toString.endsWith(".java"))
          && !excludedFiles.contains(path.getFileName.toString))
      .map(_.toString).toList
}
