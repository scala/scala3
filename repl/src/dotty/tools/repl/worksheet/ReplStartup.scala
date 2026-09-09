package dotty.tools.repl.worksheet

import dotty.tools.repl.ReplDriver
import dotty.tools.repl.ScalaClassLoader.fromURLsParallelCapable
import dotty.tools.repl.State

import java.io.ByteArrayOutputStream
import java.io.File
import java.io.PrintStream
import java.nio.charset.StandardCharsets

private final class ReplStartup(settings: Array[String]):
  private val buffer = new ByteArrayOutputStream
  private val sink = new PrintStream(buffer, true, StandardCharsets.UTF_8)

  private val runtimeLoader: ClassLoader = ReplStartup.runtimeLoader(settings)

  val driver: ReplDriver =
    Console.withOut(sink):
      Console.withErr(sink):
        new ReplDriver(
          settings :+ "-Xrepl-interrupt-instrumentation:local",
          sink,
          Some(runtimeLoader)
        )

  val isUsable: Boolean = driver.replShouldStart

  val initialState: State =
    if isUsable then driver.initialState
    else State(0, 0, Map.empty, Set.empty, false, driver.replRootContext)

  val diagnostics: List[WorksheetDiagnostic] =
    sink.flush()
    val reported = buffer.toString(StandardCharsets.UTF_8).linesIterator
      .map(_.trim)
      .filter(_.nonEmpty)
      .mkString("\n")
    val message =
      if reported.nonEmpty then reported
      else "The compiler rejected its configuration, so the worksheet was not evaluated."
    Option
      .when(!isUsable || reported.nonEmpty)(
        WorksheetDiagnostic(
          WorksheetPosition(0, 0, 0, 0),
          message,
          if isUsable then WorksheetDiagnosticSeverity.Warning
          else WorksheetDiagnosticSeverity.Error
        )
      )
      .toList

  def close(): Unit =
    WorksheetClassLoaders.closeCreated(driver.replRenderingClassLoader.getOrElse(runtimeLoader))
    sink.close()

private object ReplStartup:
  private def runtimeLoader(settings: Array[String]): ClassLoader =
    val (classpath, _) = WorksheetOptions.withoutClasspath(settings.toList)
    val entries = classpath.flatMap(_.split(File.pathSeparator)).filter(_.nonEmpty)
    fromURLsParallelCapable(
      entries.map(entry => new File(entry).toURI.toURL),
      getClass.getClassLoader
    )
