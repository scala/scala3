package dotty.tools
package dotc
package reporting

import scala.collection.mutable
import util.SourcePosition
import core.Contexts._
import Reporter._
import java.io.{ BufferedReader, IOException, PrintWriter }
import scala.reflect.internal.util._

/**
 * This class implements a Reporter that displays messages on a text
 * console.
 */
class ConsoleReporter(
    reader: BufferedReader = Console.in,
    writer: PrintWriter = new PrintWriter(Console.err, true))(ctx: Context)
  extends Reporter(ctx) with UniqueMessagePositions {

  /** maximal number of error messages to be printed */
  protected def ErrorLimit = 100

  def printSourceLine(pos: SourcePosition) =
    printMessage(pos.lineContents.stripLineEnd)

  def printColumnMarker(pos: SourcePosition) =
    if (pos.exists) { printMessage(" " * (pos.column - 1) + "^") }

  /** Prints the message. */
  def printMessage(msg: String) { writer.print(msg + "\n"); writer.flush() }

  /** Prints the message with the given position indication. */
  def printMessageAndPos(msg: String, pos: SourcePosition)(implicit ctx: Context) {
    val posStr = if (pos.exists) s"$pos: " else ""
    printMessage(posStr + msg)
    if (pos.exists) {
      printSourceLine(pos)
      printColumnMarker(pos)
    }
  }

  override def doReport(d: Diagnostic)(implicit ctx: Context) {
    if (d.severity != ERROR || count(d.severity.level) <= ErrorLimit)
      printMessageAndPos(label(d.severity) + d.msg, d.pos)
    if (d.severity.level > INFO.level && ctx.settings.prompt.value) displayPrompt()
  }

  def displayPrompt(): Unit = {
    writer.print("\na)bort, s)tack, r)esume: ")
    writer.flush()
    if (reader != null) {
      val response = reader.read().asInstanceOf[Char].toLower
      if (response == 'a' || response == 's') {
        (new Exception).printStackTrace()
        if (response == 'a')
          sys exit 1

        writer.print("\n")
        writer.flush()
      }
    }
  }

  override def flush() { writer.flush() }
}
