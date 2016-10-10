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
import diagnostic.{ Message, MessageContainer, NoExplanation }
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

  def sourceLines(pos: SourcePosition)(implicit ctx: Context): (List[String], List[String], Int) = {
    var maxLen = Int.MinValue
    def render(xs: List[Int]) =
      xs.map(pos.source.offsetToLine(_))
      .map { lineNbr =>
        val prefix = s"${lineNbr + 1} |"
        maxLen = math.max(maxLen, prefix.length)
        (prefix, pos.lineContent(lineNbr).stripLineEnd)
      }
      .map { case (prefix, line) =>
        val lnum = Red(" " * math.max(0, maxLen - prefix.length) + prefix)
        hl"$lnum$line"
      }

    val (before, after) = pos.beforeAndAfterPoint
    (render(before), render(after), maxLen)
  }

  def columnMarker(pos: SourcePosition, offset: Int)(implicit ctx: Context) = {
    val prefix = " " * (offset - 1)
    val whitespace = " " * pos.startColumn
    val carets = Red {
      if (pos.startLine == pos.endLine)
        "^" * math.max(1, pos.endColumn - pos.startColumn)
      else "^"
    }

    s"$prefix|$whitespace${carets.show}"
  }

  def errorMsg(pos: SourcePosition, msg: String, offset: Int)(implicit ctx: Context) = {
    val leastWhitespace = msg.lines.foldLeft(Int.MaxValue) { (minPad, line) =>
      val lineLength = stripColor(line).length
      val padding =
        math.min(math.max(0, ctx.settings.pageWidth.value - offset - lineLength), offset + pos.startColumn)

      if (padding < minPad) padding
      else minPad
    }

    msg.lines
      .map { line => " " * (offset - 1) + "|" + (" " * (leastWhitespace - offset)) + line }
      .mkString(sys.props("line.separator"))
  }

  def posStr(pos: SourcePosition, diagnosticLevel: String, message: Message)(implicit ctx: Context) =
    if (pos.exists) Blue({
      val file = pos.source.file.toString
      val errId =
        if (message.errorId != NoExplanation.ID)
          s"[E${"0" * (3 - message.errorId.toString.length) + message.errorId}] "
        else ""
      val kind =
        if (message.kind == "") diagnosticLevel
        else s"${message.kind} $diagnosticLevel"
      val prefix = s"-- ${errId}${kind}: $file "

      prefix +
      ("-" * math.max(ctx.settings.pageWidth.value - stripColor(prefix).length, 0))
    }).show else ""

  /** Prints the message with the given position indication. */
  def printMessageAndPos(msg: Message, pos: SourcePosition, diagnosticLevel: String)(implicit ctx: Context): Boolean = {
    printMessage(posStr(pos, diagnosticLevel, msg))
    if (pos.exists) {
      val (srcBefore, srcAfter, offset) = sourceLines(pos)
      val marker = columnMarker(pos, offset)
      val err = errorMsg(pos, msg.msg, offset)

      printMessage((srcBefore ::: marker :: err :: srcAfter).mkString("\n"))
    } else printMessage(msg.msg)
    true
  }

  def printExplanation(m: Message)(implicit ctx: Context): Unit = {
    printMessage(hl"""|
                      |${Blue("Explanation")}
                      |${Blue("===========")}""".stripMargin)
    printMessage(m.explanation)
    if (m.explanation.lastOption != Some('\n')) printMessage("")
  }

  override def doReport(m: MessageContainer)(implicit ctx: Context): Unit = {
    val didPrint = m match {
      case m: Error =>
        val didPrint = printMessageAndPos(m.contained, m.pos, "Error")
        if (ctx.settings.prompt.value) displayPrompt()
        didPrint
      case m: ConditionalWarning if !m.enablingOption.value =>
        false
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

    if (didPrint && ctx.shouldExplain(m))
      printExplanation(m.contained)
    else if (didPrint && m.contained.explanation.nonEmpty)
      printMessage("\nlonger explanation available when compiling with `-explain`")
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

