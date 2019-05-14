package dotty.tools
package dotc
package reporting

import java.io.{ PrintStream, PrintWriter, File => JFile, FileOutputStream, StringWriter }
import java.text.SimpleDateFormat
import java.util.Date
import core.Decorators._

import scala.collection.mutable

import util.SourcePosition
import core.Contexts._
import Reporter._
import diagnostic.{ Message, MessageContainer, NoExplanation }
import diagnostic.messages._
import interfaces.Diagnostic.{ ERROR, WARNING, INFO }

class TestReporter protected (outWriter: PrintWriter, filePrintln: String => Unit, logLevel: Int)
extends Reporter with UniqueMessagePositions with HideNonSensicalMessages with MessageRendering {
  import MessageContainer._

  protected final val _errorBuf = mutable.ArrayBuffer.empty[MessageContainer]
  final def errors: Iterator[MessageContainer] = _errorBuf.iterator

  protected final val _messageBuf = mutable.ArrayBuffer.empty[String]
  final def messages: Iterator[String] = _messageBuf.iterator

  protected final val _consoleBuf = new StringWriter
  protected final val _consoleReporter = new ConsoleReporter(null, new PrintWriter(_consoleBuf))
  final def consoleOutput: String = _consoleBuf.toString

  private[this] var _didCrash = false
  final def compilerCrashed: Boolean = _didCrash

  protected final def inlineInfo(pos: SourcePosition)(implicit ctx: Context): String =
    if (pos.exists) {
      if (pos.outer.exists)
        i"\ninlined at ${pos.outer}:\n" + inlineInfo(pos.outer)
      else ""
    }
    else ""

  def log(msg: String) =
    _messageBuf.append(msg)

  def logStackTrace(thrown: Throwable): Unit = {
    _didCrash = true
    val sw = new java.io.StringWriter
    val pw = new java.io.PrintWriter(sw)
    thrown.printStackTrace(pw)
    log(sw.toString)
  }

  /** Prints the message with the given position indication. */
  def printMessageAndPos(m: MessageContainer, extra: String)(implicit ctx: Context): Unit = {
    val msg = messageAndPos(m.contained(), m.pos, diagnosticLevel(m))
    val extraInfo = inlineInfo(m.pos)

    if (m.level >= logLevel) {
      outWriter.println(msg)
      if (extraInfo.nonEmpty) outWriter.println(extraInfo)
    }

    _messageBuf.append(msg)
    if (extraInfo.nonEmpty) _messageBuf.append(extraInfo)
  }

  override def doReport(m: MessageContainer)(implicit ctx: Context): Unit = {

    // Here we add extra information that we should know about the error message
    val extra = m.contained() match {
      case pm: PatternMatchExhaustivity => s": ${pm.uncovered}"
      case _ => ""
    }

    m match {
      case m: Error => {
        _errorBuf.append(m)
        _consoleReporter.doReport(m)
        printMessageAndPos(m, extra)
      }
      case m =>
        printMessageAndPos(m, extra)
    }
  }
}

object TestReporter {
  private[this] var outFile: JFile = _
  private[this] var logWriter: PrintWriter = _

  private[this] def initLog() = if (logWriter eq null) {
    val date = new Date
    val df0 = new SimpleDateFormat("yyyy-MM-dd")
    val df1 = new SimpleDateFormat("yyyy-MM-dd-'T'HH-mm-ss")
    val folder = s"testlogs/tests-${df0.format(date)}"
    new JFile(folder).mkdirs()
    outFile = new JFile(s"$folder/tests-${df1.format(date)}.log")
    logWriter = new PrintWriter(new FileOutputStream(outFile, true))
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
    new TestReporter(new PrintWriter(ps, true), logPrintln, logLevel)

  def simplifiedReporter(writer: PrintWriter): TestReporter = {
    val rep = new TestReporter(writer, logPrintln, WARNING) {
      /** Prints the message with the given position indication in a simplified manner */
      override def printMessageAndPos(m: MessageContainer, extra: String)(implicit ctx: Context): Unit = {
        def report() = {
          val msg = s"${m.pos.line + 1}: " + m.contained().kind + extra
          val extraInfo = inlineInfo(m.pos)

          writer.println(msg)
          _messageBuf.append(msg)

          if (extraInfo.nonEmpty) {
            writer.println(extraInfo)
            _messageBuf.append(extraInfo)
          }
        }
        m match {
          case m: Error => report()
          case m: Warning => report()
          case _ => ()
        }
      }
    }
    rep
  }
}
