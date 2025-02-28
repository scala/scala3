package dotty
package tools
package scripting

import scala.language.unsafeNulls

import java.nio.file.Files, java.nio.charset.StandardCharsets.UTF_8
import org.junit.{ After, Test }
import org.junit.Assert.assertEquals
import org.junit.Assume.assumeFalse
import org.junit.experimental.categories.Category

import ScriptTestEnv.*

@Category(Array(classOf[BootstrappedOnlyTests]))
class BashExitCodeTests:
  private var myTmpDir: String | Null = null
  private lazy val tmpDir = { myTmpDir = Files.createTempDirectory("exit-code-tests").toFile.absPath; myTmpDir }
  @After def cleanup(): Unit = {
    if myTmpDir != null then io.Directory(myTmpDir).deleteRecursively()

    cleanupScalaCLIDirs()
  }

  /** Verify the exit code of running `cmd args*`. */
  def verifyExit(cmd: String, args: String*)(expectedExitCode: Int): Unit =
    assumeFalse("Scripts do not yet support Scala 2 library TASTy", Properties.usingScalaLibraryTasty)
    val (validTest, exitCode, stdout, stderr) = bashCommand((cmd +: args).mkString(" "))
    if verifyValid(validTest) then
      assertEquals({
        def pp(n: String, ss: Seq[String]) = if ss.isEmpty then "" else s"\nstd$n:${ss.map("\n  " + _).mkString}"
        s"expected $expectedExitCode but got $exitCode${pp("out", stdout)}${pp("err", stderr)}"
      }, expectedExitCode, exitCode)

  // Helpers for running scala, scalac and repl without the output directory ("raw")
  def scala(args: String*)     = verifyExit(scalaPath, ("--power" +: args :+ "--offline" :+ "--server=false")*)
  def scalacRaw(args: String*) = verifyExit(scalacPath, args*)
  def scalac(args: String*)    = scalacRaw(("-d" +: tmpDir +: args)*)
  def repl(args: String*)      = verifyExit(scalaPath, ("--power" +: "repl" +: "--offline" +: "--" +: args)*)

  /** The path to the test file for this class. */
  def f(body: String, suffix: String = ".scala"): String =
    Files.write(Files.createTempFile(tmpDir.toPath, getClass.getSimpleName, suffix), body.getBytes(UTF_8)).absPath

  @Test def neg = scalac(f("@main def Test = prin"))(1)
  @Test def run = scalac(f("@main def Test = ???"))(0) & scala("-classpath", tmpDir, "-M", "Test")(1)
  @Test def pos = scalac(f("@main def Test = ()"))(0) & scala("-classpath", tmpDir, "-M", "Test")(0)

  @Test def runNeg_script = scala(f("prin", ".sc"))(1)
  @Test def runRun_script = scala(f("???", ".sc"))(1)
  @Test def runPos_script = scala(f("()", ".sc"))(0)

  @Test def runNeg = scala(f("@main def Test = prin", ".scala"))(1)
  @Test def runRun = scala(f("@main def Test = ???", ".scala"))(1)
  @Test def runPos = scala(f("@main def Test = ()", ".scala"))(0)

  @Test def scNeg = scalac("-script", f("@main def Test = prin", ".sc"))(1)
  @Test def scRun = scalac("-script", f("@main def Test = ???", ".sc"))(1)
  @Test def scPos = scalac("-script", f("@main def Test = ()", ".sc"))(0)

  @Test def evalNeg = scala("-e", "'prinln(10*10)'")(1)
  @Test def evalRun = scala("-e", "'1/0'")(1)
  @Test def evalPos = scala("-e", "'println(10*10)'")(0)

  @Test def decompileNeg  = scalac("-decompile", "non-existing-file.tasty")(1)
  @Test def decompilePos  = scalac(f("class Test"))(0) & scalacRaw("-decompile", s"$tmpDir/Test.tasty")(0)

  @Test def printTastyNeg = scalac("-print-tasty", "non-existing-file.tasty")(1)
  @Test def printTastyPos = scalac(f("class Test"))(0) & scalacRaw("-print-tasty", s"$tmpDir/Test.tasty")(0)

  @Test def help          = scala("--help")(0)
  @Test def version       = scala("--version")(0)
  @Test def xPluginList   = scala("-Xplugin-list")(0)
  @Test def vPhases       = scala("-Vphases")(0)

  @Test def replEval      = repl("--repl-quit-after-init", "--repl-init-script", "\'println(\"Hello from init script!\"); val i = 2 * 2\'")(0)

  /** A utility for running two commands in a row, like you do in bash. */
  extension (inline u1: Unit) inline def & (inline u2: Unit): Unit = { u1; u2 }
end BashExitCodeTests
