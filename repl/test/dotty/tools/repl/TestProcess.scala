package dotty.tools
package repl

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import scala.concurrent.duration.*
import scala.util.control.NonFatal

private[repl] object TestProcess:
  private val Timeout = 5.minutes

  def output(command: Seq[String]): String =
    val outputFile = Files.createTempFile("dotty-test-process-", ".log")
    val process = start(command, outputFile)

    try awaitOutput(command, process, outputFile)
    finally cleanup(process, outputFile)

  private def start(command: Seq[String], outputFile: Path): Process =
    try
      ProcessBuilder(command*).redirectErrorStream(true).redirectOutput(outputFile.toFile).start()
    catch
      case NonFatal(error) =>
        Files.deleteIfExists(outputFile)
        throw error

  private def awaitOutput(command: Seq[String], process: Process, outputFile: Path): String =
    if !process.waitFor(Timeout.length, Timeout.unit) then
      process.destroyForcibly().waitFor()
      throw new AssertionError(
        s"Command timed out after $Timeout: ${command.mkString(" ")}\n${Files.readString(outputFile, UTF_8)}"
      )

    val output = Files.readString(outputFile, UTF_8)
    if process.exitValue != 0 then
      throw new AssertionError(s"Command failed: ${command.mkString(" ")}\n$output")
    output

  private def cleanup(process: Process, outputFile: Path): Unit =
    if process.isAlive then process.destroyForcibly().waitFor()
    Files.deleteIfExists(outputFile)

private[repl] object ReplTestProcess:
  def javaHomeOverride: Option[String] =
    sys.props.get("dotty.tests.replJavaHome").orElse(sys.env.get("DOTTY_REPL_TEST_JAVA_HOME"))

  def output(initScript: String): String =
    TestProcess.output(command(Seq("--repl-quit-after-init", "--repl-init-script", initScript)))

  private def command(arguments: Seq[String]): Seq[String] =
    val javaHome = javaHomeOverride.getOrElse(sys.props("java.home"))
    Seq(
      new File(javaHome, "bin/java").toString,
      "-Dscala.usejavacp=true",
      "-classpath", sys.props("java.class.path").nn,
      "dotty.tools.repl.Main",
      "-usejavacp",
      "-color:never",
    ) ++ arguments
