package dotty.tools
package dotc
package reporting

import scala.collection.mutable
import util.SourcePosition
import core.Contexts._, core.Decorators._
import Reporter._
import java.io.{ BufferedReader, IOException, PrintWriter }
import scala.reflect.internal.util._
import printing.SyntaxHighlighting._
import printing.Highlighting._
import diagnostic.{ Message, MessageContainer }
import diagnostic.messages._

/**
  * This class implements a Reporter that displays messages on a text console
  */
class ConsoleReporter(
  reader: BufferedReader = Console.in,
  writer: PrintWriter = new PrintWriter(Console.err, true)
) extends Reporter with UniqueMessagePositions with HideNonSensicalMessages {

  import MessageContainer._

  /** maximal number of error messages to be printed */
  protected def ErrorLimit = 100

  /** Prints the message. */
  def printMessage(msg: String): Unit = { writer.print(msg + "\n"); writer.flush() }

  def stripColor(str: String): String =
    str.replaceAll("\u001B\\[[;\\d]*m", "")

  def sourceLine(pos: SourcePosition)(implicit ctx: Context): (String, Int) = {
    val lineNum = s"${pos.line}:"
    (lineNum + hl"${pos.lineContent.stripLineEnd}", lineNum.length)
  }

  def columnMarker(pos: SourcePosition, offset: Int)(implicit ctx: Context) =
    if (pos.startLine == pos.endLine) {
      val whitespace = " " * (pos.startColumn + offset)
      val carets =
        Red("^" * math.max(1, pos.endColumn - pos.startColumn))

      whitespace + carets.show
    } else {
      Red(" " * (pos.column + offset) + "^").show
    }

  def errorMsg(pos: SourcePosition, msg: String, offset: Int)(implicit ctx: Context) = {
    var hasLongLines = false
    val leastWhitespace = msg.lines.foldLeft(Int.MaxValue) { (minPad, line) =>
      val lineLength = stripColor(line).length
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

  def posStr(pos: SourcePosition, diagnosticLevel: String, message: Message)(implicit ctx: Context) =
    if (pos.exists) Blue({
      val file = pos.source.file.toString
      val errId = if (message.errorId != "") s"[${message.errorId}] " else ""
      val kind =
        if (message.kind == "") diagnosticLevel
        else s"${message.kind} $diagnosticLevel"
      val prefix = s"-- ${errId}${kind}: $file "

      prefix +
      ("-" * math.max(ctx.settings.pageWidth.value - stripColor(prefix).length, 0))
    }).show else ""

  /** Prints the message with the given position indication. */
  def printMessageAndPos(msg: Message, pos: SourcePosition, diagnosticLevel: String)(implicit ctx: Context): Unit = {
    printMessage(posStr(pos, diagnosticLevel, msg))
    if (pos.exists) {
      val (src, offset) = sourceLine(pos)
      val marker = columnMarker(pos, offset)
      val err = errorMsg(pos, msg.msg, offset)

      printMessage(List(src, marker, err).mkString("\n"))
    } else printMessage(msg.msg)
  }

  def printExplanation(m: Message)(implicit ctx: Context): Unit = {
    printMessage(hl"""|
                      |${Blue("Explanation")}
                      |${Blue("===========")}""".stripMargin)
    printMessage(m.explanation)
  }

  override def doReport(m: MessageContainer)(implicit ctx: Context): Unit = {
    m match {
      case m: Error =>
        printMessageAndPos(m.contained, m.pos, "Error")
        if (ctx.settings.prompt.value) displayPrompt()
      case m: ConditionalWarning if !m.enablingOption.value =>
      case m: FeatureWarning =>
        printMessageAndPos(m.contained, m.pos, "Feature Warning")
      case m: DeprecationWarning =>
        printMessageAndPos(m.contained, m.pos, "Deprecation Warning")
      case m: UncheckedWarning =>
        printMessageAndPos(m.contained, m.pos, "Unchecked Warning")
      case m: MigrationWarning =>
        printMessageAndPos(m.contained, m.pos, "Migration Warning")
      case m: Warning =>
        printMessageAndPos(m.contained, m.pos, "Warning")
      case m: Info =>
        printMessageAndPos(m.contained, m.pos, "Info")
    }

    if (ctx.shouldExplain(m)) printExplanation(m.contained)
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

