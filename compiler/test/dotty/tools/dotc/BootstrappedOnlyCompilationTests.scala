package dotty
package tools
package dotc

import scala.language.unsafeNulls

import org.junit.{ Test, BeforeClass, AfterClass }
import org.junit.Assert._
import org.junit.Assume._
import org.junit.Ignore
import org.junit.experimental.categories.Category

import scala.concurrent.duration._
import reporting.TestReporter
import vulpix._

import java.nio.file._

@Category(Array(classOf[BootstrappedOnlyTests]))
class BootstrappedOnlyCompilationTests {
  import ParallelTesting._
  import TestConfiguration._
  import BootstrappedOnlyCompilationTests._
  import CompilationTest.aggregateTests

  // Positive tests ------------------------------------------------------------

  @Test def posMacros: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compilePosMacros")
    aggregateTests(
      compileFilesInDir("tests/bench", defaultOptions.without("-Yno-deep-subtypes")),
      compileFilesInDir("tests/pos-macros", defaultOptions.and("-Xcheck-macros")),
    ).checkCompile()
  }

  @Test def posWithCompiler: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compilePosWithCompiler")
    aggregateTests(
      compileFilesInDir("tests/pos-with-compiler", withCompilerOptions),
      compileFilesInDir("tests/pos-staging", withStagingOptions),
      compileDir("compiler/src/dotty/tools/dotc/ast", withCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/config", withCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/core", withCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/transform", withCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/parsing", withCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/printing", withCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/reporting", withCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/typer", withCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/util", withCompilerOptions),
      compileDir("compiler/src/dotty/tools/io", withCompilerOptions),
      compileDir("tasty/src/dotty/tools/tasty", withCompilerOptions),
      compileList(
        "testIssue6460",
        List(
          "compiler/src/dotty/tools/dotc/core/SymbolLoaders.scala",
          "compiler/src/dotty/tools/dotc/core/Types.scala"
        ),
        withCompilerOptions
      ),
      compileList(
        "testIssue6603",
        List(
          "compiler/src/dotty/tools/dotc/ast/Desugar.scala",
          "compiler/src/dotty/tools/dotc/ast/Trees.scala",
          "compiler/src/dotty/tools/dotc/core/Types.scala"
        ),
        withCompilerOptions
      ),
    ).checkCompile()
  }

  @Test def posTwiceWithCompiler: Unit = {
    implicit val testGroup: TestGroup = TestGroup("posTwiceWithCompiler")
    aggregateTests(
      compileFile("tests/pos-with-compiler/Labels.scala", withCompilerOptions),
      compileFile("tests/pos-with-compiler/Patterns.scala", withCompilerOptions),
      compileList(
        "testNonCyclic",
        List(
          "compiler/src/dotty/tools/dotc/CompilationUnit.scala",
          "compiler/src/dotty/tools/dotc/core/Types.scala",
          "compiler/src/dotty/tools/dotc/ast/Trees.scala"
        ),
        withCompilerOptions
      ),
      compileList(
        "testIssue34",
        List(
          "compiler/src/dotty/tools/dotc/config/Properties.scala",
          "compiler/src/dotty/tools/dotc/config/PathResolver.scala"
        ),
        withCompilerOptions
      )
    ).times(2).checkCompile()
  }

  // Negative tests ------------------------------------------------------------

  @Test def negMacros: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compileNegWithCompiler")
    compileFilesInDir("tests/neg-macros", defaultOptions.and("-Xcheck-macros"))
      .checkExpectedErrors()
  }

  @Test def negWithCompiler: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compileNegWithCompiler")
    aggregateTests(
      compileFilesInDir("tests/neg-with-compiler", withCompilerOptions),
      compileFilesInDir("tests/neg-staging", withStagingOptions),
    ).checkExpectedErrors()
  }

  // Run tests -----------------------------------------------------------------

  @Test def runMacros: Unit = {
    implicit val testGroup: TestGroup = TestGroup("runMacros")
    compileFilesInDir("tests/run-macros", defaultOptions.and("-Xcheck-macros"), FileFilter.exclude(TestSources.runMacrosScala2LibraryTastyExcludelisted))
      .checkRuns()
  }

  /** Test ALL macros with TASTy-based interpretation - finds edge cases */
  @Test def runAllMacrosTastyInterpreter: Unit = {
    implicit val testGroup: TestGroup = TestGroup("runAllMacrosTastyInterpreter")
    val tastyInterpreterOptions = defaultOptions.and("-Xcheck-macros", "-Ytasty-interpreter")
    compileFilesInDir("tests/run-macros", tastyInterpreterOptions, FileFilter.exclude(TestSources.runMacrosScala2LibraryTastyExcludelisted))
      .checkRuns()
  }

  /** Test macro execution using TASTy-based interpretation instead of JVM reflection */
  @Test def runMacrosTastyInterpreter: Unit = {
    implicit val testGroup: TestGroup = TestGroup("runMacrosTastyInterpreter")
    // Test a subset of macros with the TASTy interpreter flag enabled
    val tastyInterpreterOptions = defaultOptions.and("-Xcheck-macros", "-Ytasty-interpreter")
    aggregateTests(
      // Basic quote/splice macros
      compileDir("tests/run-macros/quote-simple-macro", tastyInterpreterOptions),
      compileDir("tests/run-macros/quote-and-splice", tastyInterpreterOptions),
      compileDir("tests/run-macros/quote-force", tastyInterpreterOptions),
      compileDir("tests/run-macros/quote-change-owner", tastyInterpreterOptions),
      compileDir("tests/run-macros/quote-whitebox", tastyInterpreterOptions),
      compileDir("tests/run-macros/quote-impure-by-name", tastyInterpreterOptions),
      compileDir("tests/run-macros/quote-inline-function", tastyInterpreterOptions),
      compileDir("tests/run-macros/quote-toExprOfSeq", tastyInterpreterOptions),
      // Inline tuples and pattern matching
      compileDir("tests/run-macros/inline-tuples-1", tastyInterpreterOptions),
      compileDir("tests/run-macros/inline-tuples-2", tastyInterpreterOptions),
      compileDir("tests/run-macros/inline-option", tastyInterpreterOptions),
      compileDir("tests/run-macros/inline-varargs-1", tastyInterpreterOptions),
      compileDir("tests/run-macros/inline-case-objects", tastyInterpreterOptions),
      // Expression mapping
      compileDir("tests/run-macros/expr-map-1", tastyInterpreterOptions),
      compileDir("tests/run-macros/expr-map-2", tastyInterpreterOptions),
      compileDir("tests/run-macros/expr-map-3", tastyInterpreterOptions),
      // Quote matching
      compileDir("tests/run-macros/quote-matcher-power", tastyInterpreterOptions),
      compileDir("tests/run-macros/quote-matcher-runtime", tastyInterpreterOptions),
      compileDir("tests/run-macros/quote-matching-optimize-1", tastyInterpreterOptions),
      compileDir("tests/run-macros/quote-matching-optimize-2", tastyInterpreterOptions),
      compileDir("tests/run-macros/quoted-matching-docs", tastyInterpreterOptions),
      // Type operations
      compileDir("tests/run-macros/from-type", tastyInterpreterOptions),
      compileDir("tests/run-macros/quote-type-matcher", tastyInterpreterOptions),
      compileDir("tests/run-macros/quote-type-matcher-2", tastyInterpreterOptions),
      // Annotation macros
      compileDir("tests/run-macros/annot-simple-fib", tastyInterpreterOptions),
      compileDir("tests/run-macros/annot-macro-main", tastyInterpreterOptions),
      compileDir("tests/run-macros/annot-bind", tastyInterpreterOptions),
      compileDir("tests/run-macros/annot-memo", tastyInterpreterOptions),
      // Class generation
      compileDir("tests/run-macros/newClass", tastyInterpreterOptions),
      compileDir("tests/run-macros/newClassExtends", tastyInterpreterOptions),
      compileDir("tests/run-macros/newClassParams", tastyInterpreterOptions),
      compileDir("tests/run-macros/newClassSelf", tastyInterpreterOptions),
      // Derivation and liftable
      compileDir("tests/run-macros/quoted-liftable-derivation-macro", tastyInterpreterOptions),
      compileDir("tests/run-macros/quoted-ToExpr-derivation-macro", tastyInterpreterOptions),
      compileDir("tests/run-macros/quoted-toExprOfClass", tastyInterpreterOptions),
      // Reflection operations
      compileDir("tests/run-macros/reflect-lambda", tastyInterpreterOptions),
      compileDir("tests/run-macros/reflect-select-copy", tastyInterpreterOptions),
      compileDir("tests/run-macros/reflect-select-copy-2", tastyInterpreterOptions),
      compileDir("tests/run-macros/reflect-inline", tastyInterpreterOptions),
      compileDir("tests/run-macros/reflect-isFunctionType", tastyInterpreterOptions),
      compileDir("tests/run-macros/reflect-sourceCode", tastyInterpreterOptions),
      // String context and interpolation
      compileDir("tests/run-macros/string-context-implicits", tastyInterpreterOptions),
      compileDir("tests/run-macros/quote-matcher-string-interpolator", tastyInterpreterOptions),
      compileDir("tests/run-macros/quote-matcher-string-interpolator-2", tastyInterpreterOptions),
      // Misc interesting cases
      compileDir("tests/run-macros/i5119", tastyInterpreterOptions),
      compileDir("tests/run-macros/i5533", tastyInterpreterOptions),
      compileDir("tests/run-macros/i6765", tastyInterpreterOptions),
      compileDir("tests/run-macros/power-macro", tastyInterpreterOptions),
      compileDir("tests/run-macros/BigFloat", tastyInterpreterOptions),
    ).checkRuns()
  }

  @Test def runWithCompiler: Unit = {
    implicit val testGroup: TestGroup = TestGroup("runWithCompiler")
    val basicTests = List(
      compileFilesInDir("tests/run-with-compiler", withCompilerOptions),
      compileFilesInDir("tests/run-staging", withStagingOptions),
      compileFilesInDir("tests/run-tasty-inspector", withTastyInspectorOptions)
    )
    aggregateTests(basicTests*).checkRuns()
  }

  @Ignore @Test def runScala2LibraryFromTasty: Unit = {
    implicit val testGroup: TestGroup = TestGroup("runScala2LibraryFromTasty")
    // These tests recompile the entire scala2-library from TASTy,
    // they are resource intensive and should not run alongside other tests to avoid timeouts
    aggregateTests(
      compileFile("tests/run-custom-args/scala2-library-from-tasty-jar.scala", withCompilerOptions),
      compileFile("tests/run-custom-args/scala2-library-from-tasty.scala", withCompilerOptions),
    ).limitThreads(2).checkRuns() // TODO reduce to limitThreads(1) if it still causes problems, this would be around 50% slower based on local benchmarking
  }

  @Test def runBootstrappedOnly: Unit = {
    implicit val testGroup: TestGroup = TestGroup("runBootstrappedOnly")
    aggregateTests(
      compileFilesInDir("tests/run-bootstrapped", withCompilerOptions),
    ).checkRuns()
  }

  // Pickling Tests ------------------------------------------------------------
  //
  // Pickling tests are very memory intensive and as such need to be run with a
  // lower level of concurrency as to not kill their running VMs

  @Test def picklingWithCompiler: Unit = {
    implicit val testGroup: TestGroup = TestGroup("testPicklingWithCompiler")
    aggregateTests(
      compileDir("compiler/src/dotty/tools", picklingWithCompilerOptions, recursive = false),
      compileDir("compiler/src/dotty/tools/dotc", picklingWithCompilerOptions, recursive = false),
      compileDir("library/src/scala/runtime/function", picklingWithCompilerOptions),
      compileFilesInDir("library/src/scala/runtime", picklingWithCompilerOptions),
      compileFilesInDir("compiler/src/dotty/tools/backend/jvm", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/ast", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/core", picklingWithCompilerOptions, recursive = false),
      compileDir("compiler/src/dotty/tools/dotc/config", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/parsing", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/printing", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/rewrites", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/transform", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/typer", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/util", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/io", picklingWithCompilerOptions),
      compileFile("tests/pos/pickleinf.scala", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/core/classfile", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/core/tasty", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/core/unpickleScala2", picklingWithCompilerOptions),
      compileDir("tasty/src/dotty/tools/tasty", picklingWithCompilerOptions)
    ).limitThreads(4).checkCompile()
  }

  @Test def testPlugins: Unit = {
    implicit val testGroup: TestGroup = TestGroup("testPlugins")
    val pluginFile = "plugin.properties"

    // 1. hack with absolute path for -Xplugin
    // 2. copy `pluginFile` to destination
    def compileFilesInDir(dir: String, run: Boolean = false): CompilationTest = {
      val outDir = defaultOutputDir + "testPlugins/"
      val sourceDir = new java.io.File(dir)

      val dirs = sourceDir.listFiles.toList.filter(_.isDirectory)
      val targets = dirs.map { dir =>
        val compileDir = createOutputDirsForDir(dir, sourceDir, outDir)
        Files.copy(dir.toPath.resolve(pluginFile), compileDir.toPath.resolve(pluginFile), StandardCopyOption.REPLACE_EXISTING)
        val flags = {
          val base = TestFlags(withCompilerClasspath, noCheckOptions).and("-Xplugin:" + compileDir.getAbsolutePath)
          if run then base.withRunClasspath(withCompilerClasspath) else base
        }
        SeparateCompilationSource("testPlugins", dir, flags, compileDir)
      }

      new CompilationTest(targets)
    }

    compileFilesInDir("tests/plugins/neg").checkExpectedErrors()
    compileDir("tests/plugins/custom/analyzer", withCompilerOptions.and("-Yretain-trees")).checkCompile()
    compileFilesInDir("tests/plugins/run", run = true).checkRuns()
  }
}

object BootstrappedOnlyCompilationTests extends ParallelTesting {
  // Test suite configuration --------------------------------------------------

  def maxDuration = 100.seconds
  def numberOfWorkers = Runtime.getRuntime().availableProcessors()
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter
  def updateCheckFiles: Boolean = Properties.testsUpdateCheckfile
  def failedTests = TestReporter.lastRunFailedTests

  implicit val summaryReport: SummaryReporting = new SummaryReport
  @AfterClass def tearDown(): Unit = {
    super.cleanup()
    summaryReport.echoSummary()
  }
}
