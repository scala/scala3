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
    writer: PrintWriter = new PrintWriter(Console.err, true))
  extends Reporter with UniqueMessagePositions with HideNonSensicalMessages {

  /** maximal number of error messages to be printed */
  protected def ErrorLimit = 100

  def printSourceLine(pos: SourcePosition) =
    printMessage(pos.lineContent.stripLineEnd)

  def printColumnMarker(pos: SourcePosition) =
    if (pos.exists) { printMessage(" " * pos.column + "^") }

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

  override def doReport(d: Diagnostic)(implicit ctx: Context): Unit = d match {
    case d: Error =>
      printMessageAndPos(s"error: ${d.message}", d.pos)
      if (ctx.settings.prompt.value) displayPrompt()
    case d: ConditionalWarning if !d.enablingOption.value =>
    case d: MigrationWarning =>
      printMessageAndPos(s"migration warning: ${d.message}", d.pos)
    case d: Warning =>
      printMessageAndPos(s"warning: ${d.message}", d.pos)
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
