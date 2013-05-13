package dotty.tools
package dotc
package reporting

import scala.collection.mutable
import util.SourcePosition
import core.Contexts._
import Reporter.Severity.{Value => Severity, _}
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

  /** Whether a short file name should be displayed before errors */
  protected def shortName: Boolean = false

  /** maximal number of error messages to be printed */
  protected def ErrorLimit = 100

  def printSourceLine(pos: SourcePosition) =
    printMessage(pos.lineContents.stripLineEnd)

  def printColumnMarker(pos: SourcePosition) =
    if (pos.exists) { printMessage(" " * (pos.column - 1) + "^") }

  /** Prints the message. */
  def printMessage(msg: String) { writer.print(msg + "\n"); writer.flush() }

  /** Prints the message with the given position indication. */
  def printMessage(msg: String, pos: SourcePosition)(implicit ctx: Context) {
    printMessage(s"${if (pos.exists) s"$pos: " else ""}$msg")
    if (pos.exists) {
      printSourceLine(pos)
      printColumnMarker(pos)
    }
  }

  def printMessage(msg: String, severity: Severity, pos: SourcePosition)(implicit ctx: Context) {
    printMessage(label(severity) + msg, pos)
  }

  override def report(msg: String, severity: Severity, pos: SourcePosition)(implicit ctx: Context) {
    if (severity != ERROR || count(severity) <= ErrorLimit)
      printMessage(msg, severity, pos)
    if (severity != INFO && ctx.settings.prompt.value) displayPrompt()
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
