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
 * This class implements a more Fancy version (with colors!) of the regular
 * `ConsoleReporter`
 */
class FancyConsoleReporter(
  reader: BufferedReader = Console.in,
  writer: PrintWriter = new PrintWriter(Console.err, true)
) extends ConsoleReporter(reader, writer) {

  def sourceLine(pos: SourcePosition)(implicit ctx: Context): (String, Int) = {
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

      val prefix = s"${Console.CYAN}-- $kind: $file "
      prefix +
      ("-" * math.max(ctx.settings.pageWidth.value - prefix.replaceAll("\u001B\\[[;\\d]*m", "").length, 0)) +
      "\n" + outer + NoColor
    }
    else ""

  /** Prints the message with the given position indication. */
  override def printMessageAndPos(msg: String, pos: SourcePosition, kind: String = "")(implicit ctx: Context): Unit = {
    printMessage(posStr(pos, kind))
    if (pos.exists) {
      val (src, offset) = sourceLine(pos)
      val marker = columnMarker(pos, offset)
      val err = errorMsg(pos, msg, offset)

      printMessage(List(src, marker, err).mkString("\n"))
    } else printMessage(msg)
  }
}
