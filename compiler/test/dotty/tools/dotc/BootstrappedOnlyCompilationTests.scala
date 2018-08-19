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
import vulpix._
import dotty.tools.io.JFile

@Category(Array(classOf[BootstrappedOnlyTests]))
class BootstrappedOnlyCompilationTests extends ParallelTesting {
  import ParallelTesting._
  import TestConfiguration._
  import CompilationTests._

  // Test suite configuration --------------------------------------------------

  def maxDuration = 30.seconds
  def numberOfSlaves = 5
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter

  // Positive tests ------------------------------------------------------------

  @Test def posWithCompiler: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compilePosWithCompiler")
    compileFilesInDir("tests/pos-with-compiler", defaultOptions) +
    compileDir("compiler/src/dotty/tools/dotc/ast", defaultOptions) +
    compileDir("compiler/src/dotty/tools/dotc/config", defaultOptions) +
    compileDir("compiler/src/dotty/tools/dotc/core", defaultOptions) +
    compileDir("compiler/src/dotty/tools/dotc/transform", defaultOptions) +
    compileDir("compiler/src/dotty/tools/dotc/parsing", defaultOptions) +
    compileDir("compiler/src/dotty/tools/dotc/printing", defaultOptions) +
    compileDir("compiler/src/dotty/tools/dotc/reporting", defaultOptions) +
    compileDir("compiler/src/dotty/tools/dotc/typer", defaultOptions) +
    compileDir("compiler/src/dotty/tools/dotc/util", defaultOptions) +
    compileDir("compiler/src/dotty/tools/io", defaultOptions) +
    compileDir("compiler/src/dotty/tools/dotc/core", TestFlags(classPath, noCheckOptions))
  }.checkCompile()

  @Test def posTwiceWithCompiler: Unit = {
    implicit val testGroup: TestGroup = TestGroup("posTwiceWithCompiler")
    compileFile("tests/pos-with-compiler/Labels.scala", defaultOptions) +
    compileFile("tests/pos-with-compiler/Patterns.scala", defaultOptions) +
    compileList(
      "testNonCyclic",
      List(
        "compiler/src/dotty/tools/dotc/CompilationUnit.scala",
        "compiler/src/dotty/tools/dotc/core/Types.scala",
        "compiler/src/dotty/tools/dotc/ast/Trees.scala"
      ),
      defaultOptions.and("-Xprompt")
    ) +
    compileList(
      "testIssue34",
      List(
        "compiler/src/dotty/tools/dotc/config/Properties.scala",
        "compiler/src/dotty/tools/dotc/config/PathResolver.scala"
      ),
      defaultOptions.and("-Xprompt")
    )
  }.times(2).checkCompile()

  // Negative tests ------------------------------------------------------------

  @Test def negAll: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compileNegWithCompiler")
    compileFilesInDir("tests/neg-with-compiler", defaultOptions)
  }.checkExpectedErrors()

  // Run tests -----------------------------------------------------------------

  @Test def runWithCompiler: Unit = {
    implicit val testGroup: TestGroup = TestGroup("runWithCompiler")
    compileFilesInDir("tests/run-with-compiler", defaultRunWithCompilerOptions) +
    compileFile("tests/run-with-compiler-custom-args/staged-streams_1.scala", defaultRunWithCompilerOptions without "-Yno-deep-subtypes")
  }.checkRuns()

  // Pickling Tests ------------------------------------------------------------
  //
  // Pickling tests are very memory intensive and as such need to be run with a
  // lower level of concurrency as to not kill their running VMs

  @Test def picklingWithCompiler: Unit = {
    implicit val testGroup: TestGroup = TestGroup("testPicklingWithCompiler")
    compileDir("compiler/src/dotty/tools", picklingOptions, recursive = false) +
    compileDir("compiler/src/dotty/tools/dotc", picklingOptions, recursive = false) +
    compileDir("library/src/dotty/runtime", picklingOptions) +
    compileDir("compiler/src/dotty/tools/backend/jvm", picklingOptions) +
    compileDir("compiler/src/dotty/tools/dotc/ast", picklingOptions) +
    compileDir("compiler/src/dotty/tools/dotc/core", picklingOptions, recursive = false) +
    compileDir("compiler/src/dotty/tools/dotc/config", picklingOptions) +
    compileDir("compiler/src/dotty/tools/dotc/parsing", picklingOptions) +
    compileDir("compiler/src/dotty/tools/dotc/printing", picklingOptions) +
    compileDir("compiler/src/dotty/tools/repl", picklingOptions) +
    compileDir("compiler/src/dotty/tools/dotc/rewrites", picklingOptions) +
    compileDir("compiler/src/dotty/tools/dotc/transform", picklingOptions) +
    compileDir("compiler/src/dotty/tools/dotc/typer", picklingOptions) +
    compileDir("compiler/src/dotty/tools/dotc/util", picklingOptions) +
    compileDir("compiler/src/dotty/tools/io", picklingOptions) +
    compileFile("tests/pos/pickleinf.scala", picklingOptions) +
    compileDir("compiler/src/dotty/tools/dotc/core/classfile", picklingOptions) +
    compileDir("compiler/src/dotty/tools/dotc/core/tasty", picklingOptions) +
    compileDir("compiler/src/dotty/tools/dotc/core/unpickleScala2", picklingOptions)
  }.limitThreads(4).checkCompile()
}
