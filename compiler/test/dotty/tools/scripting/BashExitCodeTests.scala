package dotty
package tools
package scripting

import scala.language.unsafeNulls

import java.io.File
import java.nio.file.Files
import org.junit.Test
import org.junit.Assert.assertEquals

import ScriptTestEnv.*


object BashExitCodeTests:
  def testFiles = scripts("/scripting/exit-code-tests")

  /*
   * Compiles the class checking exit code
   *
   * @param testName name of the test containing the extension
   * @param expectedExitCode expected exit code from the output
   * @param deleteTempDir if temporary directory created for compilation output should delete in this scope
   *
   * @return Created temporary directory
   */
  private def testCompilationExitCode(testName: String, expectedExitCode: Int, deleteTempDir: Boolean = false): File =
    val temporaryDir = Files.createTempDirectory(testName)
    assertTestExists(testName) { testFile =>
      try {
        val testFilePath = testFile.absPath
        val commandline = (Seq(scalacPath, "-d", temporaryDir, testFilePath)).mkString(" ")
        val (validTest, exitCode, _, _) = bashCommand(commandline)
        if verifyValid(validTest) then
          assertEquals(expectedExitCode, exitCode)
      } finally {
        if deleteTempDir then Util.deleteFile(temporaryDir.toFile)
      }
    }
    temporaryDir.toFile

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


  /* Compiles and then runs the code checking the exit code
   *
   * @param testFileName name of the test
   * @param runExitCode expected exit code from the runner output
   */
  private def testCompileAndRun(testFileName: String, runExitCode: Int): Unit =
    val outputDirectory = testCompilationExitCode(testFileName, 0)

    def testRuntimeExitCode(className: String, expectedExitCode: Int): Unit =
        val testClassFile = outputDirectory.files.find(_.getName == s"$className.class")
        assert(testClassFile.isDefined)
        val commandline = (Seq(scalaPath, "-classpath", outputDirectory.getAbsolutePath, className)).mkString(" ")
        val (validTest, exitCode, o, e) = bashCommand(commandline)
          if verifyValid(validTest) then
            assertEquals(expectedExitCode, exitCode)

    try {
      val generatedClassName = testFileName.split('.').head
      testRuntimeExitCode(generatedClassName,  runExitCode)
    } finally {
      Util.deleteFile(outputDirectory)
    }

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

class BashExitCodeTests:
  import BashExitCodeTests.*

  @Test def verifyExitCodeOnCompileError: Unit =
    testCompilationExitCode("compileError.scala", 1, true)

  @Test def verifyExitCodeOnRuntimeError: Unit =
    testCompileAndRun("runtimeError.scala", 1)

  @Test def verifyExitCode: Unit =
    testCompileAndRun("positiveTest.scala", 0)

  @Test def verifyExitCodeOnScriptError: Unit =
    assertTestExists("scriptRuntimeError.sc") { file =>
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
    val testName = "positiveTest"
    val outputDirectory = testCompilationExitCode(s"$testName.scala", 0)
    val tastyFilePath =  outputDirectory.toPath.resolve(s"$testName.tasty").toString
    testCommandExitCode(Seq(scalacPath, "-decompile", tastyFilePath), 0)

  @Test def verifyExitCodeOnPrintTasty: Unit =
    val testName = "positiveTest"
    val outputDirectory = testCompilationExitCode(s"$testName.scala", 0)
    val tastyFilePath =  outputDirectory.toPath.resolve(s"$testName.tasty").toString
    testCommandExitCode(Seq(scalacPath, "-print-tasty", tastyFilePath), 0)

  @Test def verifyExitCodeOnDecompilationFailure: Unit =
    val tempFile = Files.createTempFile("temp-file", ".class").toFile
    testCommandExitCode(Seq(scalacPath, "-decompile", "non-existing-file.tasty"), 1)
    testCommandExitCode(Seq(scalacPath, "-decompile", tempFile.absPath), 1)
    Util.deleteFile(tempFile)

  @Test def verifyExitCodeOnPrintTastyFailure: Unit =
    val tempFile = Files.createTempFile("temp-file", ".class").toFile
    testCommandExitCode(Seq(scalacPath, "-print-tasty", "non-existing-file.tasty"), 1)
    testCommandExitCode(Seq(scalacPath, "-print-tasty", tempFile.absPath), 1)
    Util.deleteFile(tempFile)

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
