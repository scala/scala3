package dotty
package tools
package scripting

import scala.language.unsafeNulls

import java.io.File
import java.nio.file.Files
import org.junit.Test
import org.junit.Assert.assertEquals
import org.junit.experimental.categories.Category

import ScriptTestEnv.*


object BashExitCodeTests:
  private def testFiles = scripts("/scripting/exit-code-tests")

  /*
   * Compiles the class checking exit code
   *
   * @param testName name of the test
   * @param expectedExitCode expected exit code from the output
   */
  private def compileAndVerifyExitCode(
    testName: String,
    expectedExitCode: Int,
  )(using temporaryDir: File): Unit =
    assertTestExists(testName) { testFile =>
      val testFilePath = testFile.absPath
      val commandline = (Seq(scalacPath, "-d", temporaryDir.absPath, testFilePath)).mkString(" ")
      val (validTest, exitCode, _, _) = bashCommand(commandline)
      if verifyValid(validTest) then
        assertEquals(expectedExitCode, exitCode)
    }

  /*
   * Runs compiled code checking the exit code
   *
   * @param className name of compiled class
   * @param runExitCode expected exit code from the runner
   */
  private def runClassAndVerifyExitCode(
    className: String,
    expectedExitCode: Int
  )(using temporaryDir: File): Unit =
    val testClassFile = temporaryDir.files.find(_.getName == s"$className.class")
    assert(testClassFile.isDefined)
    val commandline = (Seq(scalaPath, "-classpath", temporaryDir.absPath, className)).mkString(" ")
    val (validTest, exitCode, o, e) = bashCommand(commandline)
      if verifyValid(validTest) then
        assertEquals(expectedExitCode, exitCode)

  /*
   * Compiles and then runs code verifying runner status code
   *
   * @param testName name of the test
   * @param className name of compiled class
   * @param expectedRunExitCode expected exit code from the runner
   */
  private def compileRunAndVerifyExitCode(
    testName: String,
    className: String,
    expectedRunExitCode: Int,
  )(using File): Unit =
    compileAndVerifyExitCode(testName, 0)
    runClassAndVerifyExitCode(className, expectedRunExitCode)

  /*
   * Runs the command and checks the exit code
   *
   * @param args arguments for command line
   * @param expectedExitCode expected exit code from the output
   */
  private def testCommandExitCode(args: Seq[String], expectedExitCode: Int): Unit =
    val commandline = args.mkString(" ")
    val (validTest, exitCode, output, erroutput) = bashCommand(commandline)
    if verifyValid(validTest) then
      assertEquals(expectedExitCode, exitCode)

  /*
   * Checks if scripting test resources contains test with given `testName`
   * And then runs function `test`
   *
   * @param testName name of the test containing the extension
   * @param test check to be run on found test file
   */
  private def assertTestExists(testName: String)(test: File => Unit) =
    val file = testFiles.find(_.getName == testName)
    assert(file.isDefined)
    test(file.get)

  /*
   * Runs test for created temporary file
   * and ensures it deletion after function execution
   *
   * @param test check to be run on found test file
   */
  private def withTempFile(test: File => Unit) =
    val tempFile = Files.createTempFile("temp-file", ".class").toFile
    try {
      test(tempFile)
    } finally {
      Util.deleteFile(tempFile)
    }

  /*
   * Runs test with implicit temporary directory
   * and ensures it deletion after the function execution
   *
   * @param test test to be run with given temporary directory
   */
  private def withTempDirectory(test: File ?=> Unit) =
    given file: File = Files.createTempDirectory("exit-code-tests").toFile
    try { test } finally { Util.deleteFile(file) }

  /*
   * Returns path to the generated tasty file for given directory and classname
   */
  private def getGeneratedTastyPath(className: String)(using temporaryDir: File): String =
    temporaryDir.toPath.resolve(s"$className.tasty").toString

@Category(Array(classOf[BootstrappedOnlyTests]))
class BashExitCodeTests:
  import BashExitCodeTests.*

  @Test def verifyExitCodeOnCompileError: Unit =
    withTempDirectory(compileAndVerifyExitCode("compileError.scala", 1))

  @Test def verifyExitCodeOnRuntimeError: Unit =
    withTempDirectory(compileRunAndVerifyExitCode("runtimeError.scala", "runtimeError", 1))

  @Test def verifyExitCode: Unit =
    withTempDirectory(compileRunAndVerifyExitCode("positiveTest.scala", "positiveTest", 0))

  @Test def verifyExitCodeOnScriptError: Unit =
    assertTestExists("scriptRuntimeError.sc"){ file =>
      testCommandExitCode(Seq(scalacPath, "-script", file.absPath), 1)
    }

  @Test def verifyExitCodeOnScriptErrorCompiler: Unit =
    assertTestExists("scriptRuntimeError.sc") { file =>
      testCommandExitCode(Seq(scalacPath, "-script", file.absPath), 1)
    }

  @Test def verifyExitCodeOnScript: Unit =
    assertTestExists("scriptPositive.sc") { file =>
      testCommandExitCode(Seq(scalaPath, file.absPath), 0)
    }

  @Test def verifyExitCodeOnScriptCompiler: Unit =
    assertTestExists("scriptPositive.sc") { file =>
      testCommandExitCode(Seq(scalacPath, "-script", file.absPath), 0)
    }

  @Test def verifyExitCodeOnDecompilation: Unit =
    withTempDirectory {
      compileAndVerifyExitCode("positiveTest.scala", 0)
      testCommandExitCode(Seq(scalacPath, "-decompile", getGeneratedTastyPath("positiveTest")), 0)
    }

  @Test def verifyExitCodeOnPrintTasty: Unit =
    withTempDirectory {
      compileAndVerifyExitCode("positiveTest.scala", 0)
      testCommandExitCode(Seq(scalacPath, "-print-tasty", getGeneratedTastyPath("positiveTest")), 0)
    }

  @Test def verifyExitCodeOnDecompilationFailure: Unit =
    withTempFile(file => testCommandExitCode(Seq(scalacPath, "-decompile", file.absPath), 1))
    testCommandExitCode(Seq(scalacPath, "-decompile", "non-existing-file.tasty"), 1)

  @Test def verifyExitCodeOnPrintTastyFailure: Unit =
    withTempFile(file => testCommandExitCode(Seq(scalacPath, "-print-tasty", file.absPath), 1))
    testCommandExitCode(Seq(scalacPath, "-print-tasty", "non-existing-file.tasty"), 1)

  @Test def verifyExitCodeOnExpressionCompileError: Unit =
    testCommandExitCode(Seq(scalaPath, "-e", "'prinln(10*10)'"), 1)

  @Test def verifyExitCodeOnExpressionRuntimeError: Unit =
    testCommandExitCode(Seq(scalaPath, "-e", "'1/0'"), 1)

  @Test def verifyExitCodeOnExpression: Unit =
    testCommandExitCode(Seq(scalaPath, "-e", "'println(10*10)'"), 0)

  @Test def verifyExitCodeOnInfo: Unit =
    List("--help", "--version", "-Xplugin-list", "-Vphases").foreach { flag =>
      testCommandExitCode(Seq(scalaPath, flag), 0)
    }
