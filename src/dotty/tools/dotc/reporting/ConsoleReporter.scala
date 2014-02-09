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
  extends Reporter with UniqueMessagePositions {

  /** maximal number of error messages to be printed */
  protected def ErrorLimit = 100

  def printSourceLine(pos: SourcePosition) =
    printMessage(pos.lineContents.stripLineEnd)

  def printColumnMarker(pos: SourcePosition) =
    if (pos.exists) { printMessage(" " * (pos.column - 1) + "^") }

  /** Prints the message. */
  def printMessage(msg: String): Unit = { writer.print(msg + "\n"); writer.flush() }

  /** Prints the message with the given position indication. */
  def printMessageAndPos(msg: String, pos: SourcePosition)(implicit ctx: Context): Unit = {
    val posStr = if (pos.exists) s"$pos: " else ""
    printMessage(posStr + msg)
    if (pos.exists) {
      printSourceLine(pos)
      printColumnMarker(pos)
    }
  }

  override def doReport(d: Diagnostic)(implicit ctx: Context): Unit =
    if (d.severity != ERROR || count(d.severity.level) <= ErrorLimit && !d.isSuppressed) {
      printMessageAndPos(label(d.severity) + d.msg, d.pos)
      if (d.severity == ERROR && ctx.settings.prompt.value) displayPrompt()
    }

  def displayPrompt(): Unit = {
    writer.print("\na)bort, s)tack, r)esume: ")
    writer.flush()
    if (reader != null) {
      val response = reader.read().asInstanceOf[Char].toLower
      if (response == 'a' || response == 's') {
        (new Exception).printStackTrace()
        if (response == 'a')
          sys.exit(1)
      }
      writer.print("\n")
      writer.flush()
    }
  }

  override def flush()(implicit ctx: Context): Unit = { writer.flush() }
}
