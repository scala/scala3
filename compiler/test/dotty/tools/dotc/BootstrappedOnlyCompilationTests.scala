package dotty
package tools
package dotc

import org.junit.{ Test, BeforeClass, AfterClass }
import org.junit.Assert._
import org.junit.Assume._
import org.junit.experimental.categories.Category

import scala.concurrent.duration._
import vulpix._

import java.nio.file._

@Category(Array(classOf[BootstrappedOnlyTests]))
class BootstrappedOnlyCompilationTests extends ParallelTesting {
  import ParallelTesting._
  import TestConfiguration._
  import BootstrappedOnlyCompilationTests._
  import CompilationTest.aggregateTests

  // Test suite configuration --------------------------------------------------

  def maxDuration = 30.seconds
  def numberOfSlaves = 5
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter
  def updateCheckFiles: Boolean = Properties.testsUpdateCheckfile

  // Positive tests ------------------------------------------------------------

  @Test def posMacros: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compilePosMacros")
    compileFilesInDir("tests/pos-macros", defaultOptions).checkCompile()
  }

  @Test def posWithCompiler: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compilePosWithCompiler")
    aggregateTests(
      compileFilesInDir("tests/pos-with-compiler", withCompilerOptions),
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
      compileList(
        "testIssue6460",
        List(
          "compiler/src/dotty/tools/dotc/core/SymbolLoaders.scala",
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
    compileFilesInDir("tests/neg-macros", defaultOptions).checkExpectedErrors()
  }

  @Test def negWithCompiler: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compileNegWithCompiler")
    compileFilesInDir("tests/neg-with-compiler", withCompilerOptions).checkExpectedErrors()
  }

  // Run tests -----------------------------------------------------------------

  @Test def runMacros: Unit = {
    implicit val testGroup: TestGroup = TestGroup("runMacros")
    compileFilesInDir("tests/run-macros", defaultOptions)
  }.checkRuns()

  @Test def runWithCompiler: Unit = {
    implicit val testGroup: TestGroup = TestGroup("runWithCompiler")
    aggregateTests(
      compileFilesInDir("tests/run-with-compiler", withCompilerOptions),
      compileDir("tests/run-with-compiler-custom-args/tasty-interpreter", withCompilerOptions),
      compileFile("tests/run-with-compiler-custom-args/staged-streams_1.scala", withCompilerOptions without "-Yno-deep-subtypes")
    ).checkRuns()
  }

  // Pickling Tests ------------------------------------------------------------
  //
  // Pickling tests are very memory intensive and as such need to be run with a
  // lower level of concurrency as to not kill their running VMs

  @Test def picklingWithCompiler: Unit = {
    val jvmBackendFilter = FileFilter.exclude(List("BTypes.scala", "Primitives.scala")) // TODO
    implicit val testGroup: TestGroup = TestGroup("testPicklingWithCompiler")
    aggregateTests(
      compileDir("compiler/src/dotty/tools", picklingWithCompilerOptions, recursive = false),
      compileDir("compiler/src/dotty/tools/dotc", picklingWithCompilerOptions, recursive = false),
      compileDir("library/src/dotty/runtime", picklingWithCompilerOptions),
      compileFilesInDir("compiler/src/dotty/tools/backend/jvm", picklingWithCompilerOptions, jvmBackendFilter),
      compileDir("compiler/src/dotty/tools/dotc/ast", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/core", picklingWithCompilerOptions, recursive = false),
      compileDir("compiler/src/dotty/tools/dotc/config", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/parsing", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/printing", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/repl", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/rewrites", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/transform", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/typer", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/util", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/io", picklingWithCompilerOptions),
      compileFile("tests/pos/pickleinf.scala", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/core/classfile", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/core/tasty", picklingWithCompilerOptions),
      compileDir("compiler/src/dotty/tools/dotc/core/unpickleScala2", picklingWithCompilerOptions)
    ).limitThreads(4).checkCompile()
  }

  @Test def testPlugins: Unit = {
    val pluginFile = "plugin.properties"

    // 1. hack with absolute path for -Xplugin
    // 2. copy `pluginFile` to destination
    def compileFilesInDir(dir: String): CompilationTest = {
      val outDir = defaultOutputDir + "testPlugins/"
      val sourceDir = new java.io.File(dir)

      val dirs = sourceDir.listFiles.toList.filter(_.isDirectory)
      val targets = dirs.map { dir =>
        val compileDir = createOutputDirsForDir(dir, sourceDir, outDir)
        Files.copy(dir.toPath.resolve(pluginFile), compileDir.toPath.resolve(pluginFile), StandardCopyOption.REPLACE_EXISTING)
        val flags = TestFlags(withCompilerClasspath, noCheckOptions).and("-Xplugin:" + compileDir.getAbsolutePath)
        SeparateCompilationSource("testPlugins", dir, flags, compileDir)
      }

      new CompilationTest(targets)
    }

    compileFilesInDir("tests/plugins/neg").checkExpectedErrors()
  }
}

object BootstrappedOnlyCompilationTests {
  implicit val summaryReport: SummaryReporting = new SummaryReport
  @AfterClass def cleanup(): Unit = summaryReport.echoSummary()
}
