package dotty.tools
package dotc
package reporting

import java.io.{ PrintWriter, File => JFile, FileOutputStream }
import java.text.SimpleDateFormat
import java.util.Date

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

  final def flushToFile(): Unit =
    _messageBuf.iterator.foreach(filePrintln)

  final def inlineInfo(pos: SourcePosition): String =
    if (pos.exists) {
      if (pos.outer.exists)
        s"\ninlined at ${pos.outer}:\n" + inlineInfo(pos.outer)
      else ""
    }
    else ""

  def echo(msg: String) =
    _messageBuf.append(msg)

  /** Prints the message with the given position indication. */
  def printMessageAndPos(m: MessageContainer, extra: String)(implicit ctx: Context): Unit = {
    val msg = messageAndPos(m.contained, m.pos, diagnosticLevel(m))
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
    val extra = m.contained match {
      case pm: PatternMatchExhaustivity => s": ${pm.uncovered}"
      case _ => ""
    }

    m match {
      case m: Error => {
        _errorBuf.append(m)
        printMessageAndPos(m, extra)
      }
      case w: Warning =>
        printMessageAndPos(w, extra)
      case _ =>
    }
  }
}

object TestReporter {
  private[this] val logWriter = {
    val df = new SimpleDateFormat("yyyy-MM-dd-HH:mm")
    val timestamp = df.format(new Date)
    new PrintWriter(new FileOutputStream(new JFile(s"../tests-$timestamp.log"), true))
  }

  def parallelReporter(lock: AnyRef, logLevel: Int): TestReporter = new TestReporter(
    new PrintWriter(Console.err, true),
    str => lock.synchronized {
      logWriter.println(str)
      logWriter.flush()
    },
    logLevel
  )

  def reporter(logLevel: Int): TestReporter = new TestReporter(
    new PrintWriter(Console.err, true),
    logWriter.println,
    logLevel
  )

  def simplifiedReporter(writer: PrintWriter): TestReporter = new TestReporter(
    writer,
    logWriter.println,
    WARNING
  ) {
    /** Prints the message with the given position indication in a simplified manner */
    override def printMessageAndPos(m: MessageContainer, extra: String)(implicit ctx: Context): Unit = {
      val msg = s"${m.pos.line + 1}: " + m.contained.kind + extra
      val extraInfo = inlineInfo(m.pos)

      writer.println(msg)
      _messageBuf.append(msg)

      if (extraInfo.nonEmpty) {
        writer.println(extraInfo)
        _messageBuf.append(extraInfo)
      }
    }
  }
}
