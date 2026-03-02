package dotty
package tools
package dotc
package reporting

import java.io.{File as JFile, *}
import java.nio.file.Files.readAllLines
import java.text.SimpleDateFormat
import java.util.Date

import core.Contexts.*
import core.Decorators.*
import interfaces.Diagnostic.{ERROR, WARNING}
import io.AbstractFile
import util.SourcePosition
import Diagnostic.*

import scala.collection.mutable
import scala.compiletime.uninitialized
import scala.io.Codec
import scala.jdk.CollectionConverters.*
import scala.language.unsafeNulls

class TestReporter protected (outWriter: PrintWriter, logLevel: Int)
extends Reporter with UniqueMessagePositions with HideNonSensicalMessages with MessageRendering {

  protected final val _diagnosticBuf = mutable.ArrayBuffer.empty[Diagnostic]
  final def diagnostics: Iterator[Diagnostic] = _diagnosticBuf.iterator
  final def errors: Iterator[Diagnostic] = diagnostics.filter(_.level >= ERROR)

  protected final val _messageBuf = mutable.ArrayBuffer.empty[String]
  final def messages: Iterator[String] = _messageBuf.iterator

  protected final val _consoleBuf = new StringWriter
  protected final val _consoleReporter = new TestConsoleReporter(new PrintWriter(_consoleBuf)):
    override protected def renderPath(file: AbstractFile): String = TestReporter.renderPath(file)

  final def consoleOutput: String = _consoleBuf.toString

  private var _skip: Boolean = false
  final def setSkip(): Unit = _skip = true
  final def skipped: Boolean = _skip

  override protected def renderPath(file: AbstractFile): String = TestReporter.renderPath(file)

  protected final def inlineInfo(pos: SourcePosition)(using Context): String =
    if (pos.exists) {
      if (pos.outer.exists)
        i"\ninlined at ${pos.outer}:\n" + inlineInfo(pos.outer)
      else ""
    }
    else ""

  def log(msg: String) =
    _messageBuf.append(msg)

  /** Prints the message with the given position indication. */
  def printMessageAndPos(dia: Diagnostic, extra: String)(using Context): Unit = {
    val msg = messageAndPos(dia)
    val extraInfo = inlineInfo(dia.pos)

    if (dia.level >= logLevel) {
      outWriter.println(msg)
      if (extraInfo.nonEmpty) outWriter.println(extraInfo)
    }

    _messageBuf.append(msg)
    if (extraInfo.nonEmpty) _messageBuf.append(extraInfo)
  }

  override def doReport(dia: Diagnostic)(using Context): Unit = {

    // Here we add extra information that we should know about the error message
    val extra = dia.msg match {
      case pm: PatternMatchExhaustivity => s": ${pm.uncovered}"
      case _ => ""
    }

    if _consoleReporter.isDiagnosticRelevant(dia) then
      _consoleReporter.doReport(dia)
      _diagnosticBuf.append(dia)
    printMessageAndPos(dia, extra)
  }

  override def printSummary()(using Context): Unit = ()
}

class TestConsoleReporter(writer: PrintWriter) extends ConsoleReporter(null, writer) {
  // Report even info-level diagnostics if they've been explicitly enabled by -Ylog
  def isDiagnosticRelevant(dia: Diagnostic)(using Context): Boolean =
    dia.level >= WARNING || ctx.settings.Ylog.value.containsPhase(ctx.phase)

  /** Print the message with the given position indication. */
  override def doReport(dia: Diagnostic)(using Context): Unit =
    if isDiagnosticRelevant(dia) then printMessage(messageAndPos(dia))
    else echoMessage(messageAndPos(dia))
}

object TestReporter {
  private val testLogsDirName: String = "testlogs"
  private val failedTestsFileName: String = "last-failed.log"
  private val failedTestsFile: JFile = new JFile(s"$testLogsDirName/$failedTestsFileName")

  private var outFile: JFile = uninitialized
  private var logWriter: PrintWriter = uninitialized
  private var failedTestsWriter: PrintWriter = uninitialized

  private def initLog() = if (logWriter eq null) {
    val date = new Date
    val df0 = new SimpleDateFormat("yyyy-MM-dd")
    val df1 = new SimpleDateFormat("yyyy-MM-dd-'T'HH-mm-ss")
    val folder = s"$testLogsDirName/tests-${df0.format(date)}"
    new JFile(folder).mkdirs()
    outFile = new JFile(s"$folder/tests-${df1.format(date)}.log")
    logWriter = new PrintWriter(new FileOutputStream(outFile, true))
    failedTestsWriter = new PrintWriter(new FileOutputStream(failedTestsFile, false))
  }

  def logPrintln(str: String) = {
    initLog()
    logWriter.println(str)
    logWriter.flush()
  }

  def logPrint(str: String): Unit = {
    initLog()
    logWriter.println(str)
  }

  def logFlush(): Unit =
    if (logWriter ne null) logWriter.flush()

  def logPath: String = {
    initLog()
    outFile.getCanonicalPath
  }

  def reporter(ps: PrintStream, logLevel: Int): TestReporter =
    new TestReporter(new PrintWriter(ps, true), logLevel)

  def simplifiedReporter(writer: PrintWriter): TestReporter = {
    val rep = new TestReporter(writer, WARNING) {
      /** Prints the message with the given position indication in a simplified manner */
      override def printMessageAndPos(dia: Diagnostic, extra: String)(using Context): Unit = {
        def report() = {
          val msg = s"${dia.pos.line + 1}: " + dia.msg.kind.message + extra
          val extraInfo = inlineInfo(dia.pos)

          writer.println(msg)
          _messageBuf.append(msg)

          if (extraInfo.nonEmpty) {
            writer.println(extraInfo)
            _messageBuf.append(extraInfo)
          }
        }
        dia match {
          case dia: Error => report()
          case dia: Warning => report()
          case _ => ()
        }
      }
    }
    rep
  }

  def lastRunFailedTests: Option[List[String]] =
    Option.when(
      Properties.rerunFailed &&
        failedTestsFile.exists() &&
        failedTestsFile.isFile
    )(readAllLines(failedTestsFile.toPath).asScala.toList)

  def writeFailedTests(tests: List[String]): Unit =
    initLog()
    tests.foreach(failed => failedTestsWriter.println(failed))
    failedTestsWriter.flush()

  def renderPath(file: AbstractFile): String =
    if JFile.separatorChar == '\\' then
      file.path.replace('\\', '/')
    else
      file.path
}
