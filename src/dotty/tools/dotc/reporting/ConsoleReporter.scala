package dotty.tools
package dotc
package reporting

import scala.collection.mutable
import util.SourcePosition
import core.Contexts._
import Reporter._
import java.io.{ BufferedReader, IOException, PrintWriter }
import scala.reflect.internal.util._
import printing.SyntaxHighlighting._

/**
 * This class implements a Reporter that displays messages on a text
 * console.
 */
class ConsoleReporter(
    reader: BufferedReader = Console.in,
    writer: PrintWriter = new PrintWriter(Console.err, true))
  extends Reporter with UniqueMessagePositions with HideNonSensicalMessages {

  /** maximal number of error messages to be printed */
  protected def ErrorLimit = 100

<<<<<<< HEAD
  def printPos(pos: SourcePosition): Unit =
    if (pos.exists) {
      printMessage(pos.lineContent.stripLineEnd)
      printMessage(" " * pos.column + "^")
      if (pos.outer.exists) {
        printMessage(s"\n... this location is in code that was inlined at ${pos.outer}:\n")
        printPos(pos.outer)
      }
    }
=======
  def sourceLine(pos: SourcePosition): (String, Int) = {
    val lineNum = s"${pos.line}:"
    (lineNum + hl"${pos.lineContent.stripLineEnd}", lineNum.length)
  }

  def columnMarker(pos: SourcePosition, offset: Int) =
    if (pos.startLine == pos.endLine) {
      val whitespace = " " * (pos.column + offset)
      val carets =
        AnnotationColor +
        ("^" * math.max(1, pos.endColumn - pos.startColumn)) +
        NoColor

      whitespace + carets
    } else {
      " " * (pos.column + offset) + AnnotationColor + "^" + NoColor
    }

  def errorMsg(pos: SourcePosition, msg: String, offset: Int)(implicit ctx: Context) = {
    var hasLongLines = false
    val leastWhitespace = msg.lines.foldLeft(Int.MaxValue) { (minPad, line) =>
      val lineLength =
        line.replaceAll("\u001B\\[[;\\d]*m", "").length
      val padding =
        math.min(math.max(0, ctx.settings.pageWidth.value - offset - lineLength), offset + pos.startColumn)

      if (padding < minPad) padding
      else minPad
    }

    msg
      .lines
      .map { line => " " * leastWhitespace + line }
      .mkString(sys.props("line.separator"))
  }

  def posStr(pos: SourcePosition, kind: String)(implicit ctx: Context) =
    if (pos.exists) {
      val file = pos.source.file.toString

      val outer = if (pos.outer.exists) {
        s"This location is in code that was inlined at ${pos.outer}:\n" +
        printStr(pos.outer) + "\n" + "-" * ctx.settings.pageWidth.value
      } else ""

      s"${Console.CYAN}$kind: $file " +
      "-" * math.max(ctx.settings.pageWidth.value - file.length - kind.length - 1, 0) +
      "\n" + outer + NoColor
    }
    else ""

  /** Prints the message. */
  def printMessage(msg: String): Unit = { writer.print(msg + "\n"); writer.flush() }

  /** Prints the message with the given position indication. */
  def printMessageAndPos(msg: String, pos: SourcePosition, kind: String = "")(implicit ctx: Context): Unit = {
    printMessage(posStr(pos, kind))
    if (pos.exists) {
      val (src, offset) = sourceLine(pos)
      val marker = columnMarker(pos, offset)
      val err = errorMsg(pos, msg, offset)

      printMessage(List(src, marker, err).mkString("\n"))
    } else printMessage(msg)
  }

  override def doReport(d: Diagnostic)(implicit ctx: Context): Unit = d match {
    case d: Error =>
      printMessageAndPos(d.message, d.pos, "Error in")
      if (ctx.settings.prompt.value) displayPrompt()
    case d: ConditionalWarning if !d.enablingOption.value =>
    case d: MigrationWarning =>
      printMessageAndPos(d.message, d.pos, "Migration Warning in")
    case d: Warning =>
      printMessageAndPos(d.message, d.pos, "Warning in")
    case _ =>
      printMessageAndPos(d.message, d.pos)
  }

  def displayPrompt(): Unit = {
    writer.print("\na)bort, s)tack, r)esume: ")
    writer.flush()
    if (reader != null) {
      val response = reader.read().asInstanceOf[Char].toLower
      if (response == 'a' || response == 's') {
        Thread.dumpStack()
        if (response == 'a')
          sys.exit(1)
      }
      writer.print("\n")
      writer.flush()
    }
  }

  override def flush()(implicit ctx: Context): Unit = { writer.flush() }
}
