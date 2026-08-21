package dotty.tools
package repl

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import scala.concurrent.duration.*
import scala.util.control.NonFatal

private[repl] object TestProcess:
  private val Timeout = 5.minutes
  private val TerminationTimeout = 10.seconds

  def output(command: Seq[String]): String =
    val outputFile = Files.createTempFile("dotty-test-process-", ".log")
    val process = start(command, outputFile)

    try
      process.getOutputStream.close()
      awaitOutput(command, process, outputFile)
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
      val terminated = terminate(process)
      val terminationFailure =
        if terminated then ""
        else s"\nProcess did not terminate within $TerminationTimeout after destroyForcibly"
      throw new AssertionError(
        s"Command timed out after $Timeout: ${command.mkString(" ")}\n${readOutput(outputFile)}$terminationFailure"
      )

    val output = readOutput(outputFile)
    if process.exitValue != 0 then
      throw new AssertionError(s"Command failed: ${command.mkString(" ")}\n$output")
    output

  private def readOutput(outputFile: Path): String =
    new String(Files.readAllBytes(outputFile), UTF_8)

  private def cleanup(process: Process, outputFile: Path): Unit =
    if terminate(process) then Files.deleteIfExists(outputFile)

  private def terminate(process: Process): Boolean =
    if !process.isAlive then true
    else
      process.destroyForcibly()
      process.waitFor(TerminationTimeout.length, TerminationTimeout.unit)

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
