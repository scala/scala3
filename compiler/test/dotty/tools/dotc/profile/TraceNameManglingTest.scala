package dotty.tools.dotc.profile

import org.junit.Assert.*
import org.junit.*

import scala.annotation.tailrec
import dotty.tools.DottyTest
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.core.Contexts.FreshContext
import java.nio.file.Files
import java.util.Locale

class TraceNameManglingTest extends DottyTest {

  override protected def initializeCtx(fc: FreshContext): Unit = {
    super.initializeCtx(fc)
    val tmpDir = Files.createTempDirectory("trace_name_mangling_test").nn
    fc.setSetting(fc.settings.YprofileEnabled, true)
    fc.setSetting(
      fc.settings.YprofileTrace,
      tmpDir.resolve("trace.json").nn.toAbsolutePath().toString()
    )
    fc.setSetting(
      fc.settings.YprofileDestination,
      tmpDir.resolve("profiler.out").nn.toAbsolutePath().toString()
    )
  }

  @Test def escapeBackslashes(): Unit = {
    val isWindows = sys.props("os.name").toLowerCase(Locale.ROOT).contains("windows")
    val filename = if isWindows then "/.scala" else "\\.scala"
    checkTraceEvents(
      """
      |class /\ :
      |  var /\ = ???
      |object /\{
      |  def /\ = ???
      |}""".stripMargin,
      filename = filename
    )(
      Set(
        raw"class /\\",
        raw"object /\\",
        raw"method /\\",
        raw"variable /\\",
        raw"setter /\\_="
      ).map(TraceEvent("typecheck", _))
        ++ Set(
          TraceEvent("file", if isWindows then "/.scala" else "\\\\.scala")
        )
    )
  }

  @Test def escapeDoubleQuotes(): Unit = {
    val filename = "\"quoted\".scala"
    checkTraceEvents(
      """
      |class `"QuotedClass"`:
      |  var `"quotedVar"` = ???
      |object `"QuotedObject"` {
      |  def `"quotedMethod"` = ???
      |}""".stripMargin,
      filename = filename
    ):
      Set(
        raw"class \"QuotedClass\"",
        raw"object \"QuotedObject\"",
        raw"method \"quotedMethod\"",
        raw"variable \"quotedVar\""
      ).map(TraceEvent("typecheck", _))
        ++ Set(TraceEvent("file", "\\\"quoted\\\".scala"))
  }
  @Test def escapeNonAscii(): Unit = {
    val filename = "unic😀de.scala"
    checkTraceEvents(
      """
      |class ΩUnicodeClass:
      |  var `中文Var` = ???
      |object ΩUnicodeObject {
      |  def 中文Method = ???
      |}""".stripMargin,
      filename = filename
    ):
      Set(
        "class \\u03A9UnicodeClass",
        "object \\u03A9UnicodeObject",
        "method \\u4E2D\\u6587Method",
        "variable \\u4E2D\\u6587Var"
      ).map(TraceEvent("typecheck", _))
        ++ Set(TraceEvent("file", "unic\\uD83D\\uDE00de.scala"))
  }

  case class TraceEvent(category: String, name: String)
  private def compileWithTracer(
      code: String,
      filename: String,
      afterPhase: String = "typer"
  )(checkEvents: Seq[TraceEvent] => Unit) = {
    val runCtx = locally:
      val source = SourceFile.virtual(filename, code)
      val c = compilerWithChecker(afterPhase) { (_, _) => () }
      val run = c.newRun
      run.compileSources(List(source))
      run.runContext
    assert(!runCtx.reporter.hasErrors, "compilation failed")
    val outfile = ctx.settings.YprofileTrace.value
    checkEvents:
      scala.io.Source
        .fromFile(outfile)
        .getLines()
        .collect:
          case s"""${_}"cat":"${category}","name":${name},"ph":${_}""" =>
            TraceEvent(category, name.stripPrefix("\"").stripSuffix("\""))
        .distinct.toSeq
  }

  private def checkTraceEvents(code: String, filename: String = "test")(expected: Set[TraceEvent]): Unit = {
    compileWithTracer(code, filename = filename, afterPhase = "typer"){ events =>
      val missing = expected.diff(events.toSet)
      def showFound = events
        .groupBy(_.category)
        .collect:
          case (category, events)
              if expected.exists(_.category == category) =>
            s"- $category: [${events.map(_.name).mkString(", ")}]"
        .mkString("\n")
      assert(
        missing.isEmpty,
        s"""Missing ${missing.size} names [${missing.mkString(", ")}] in events, got:\n${showFound}"""
      )
    }
  }
}
