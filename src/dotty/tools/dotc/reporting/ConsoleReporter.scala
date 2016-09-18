package dotty.tools
package dotc
package reporting

import scala.collection.mutable
import util.SourcePosition
import core.Contexts._
import Reporter._
import java.io.{ BufferedReader, IOException, PrintWriter }
import scala.reflect.internal.util._
import diagnostic.Message
import diagnostic.messages._

/**
 * This class implements a Reporter that displays messages on a text
 * console.
 */
class ConsoleReporter(
    reader: BufferedReader = Console.in,
    writer: PrintWriter = new PrintWriter(Console.err, true))
  extends Reporter with UniqueMessagePositions with HideNonSensicalMessages {

  import Message._

  /** maximal number of error messages to be printed */
  protected def ErrorLimit = 100

  def printSourceLine(pos: SourcePosition) =
    printMessage(pos.lineContent.stripLineEnd)

  def printColumnMarker(pos: SourcePosition) =
    if (pos.exists) { printMessage(" " * pos.column + "^") }

  /** Prints the message. */
  def printMessage(msg: String): Unit = { writer.print(msg + "\n"); writer.flush() }

  /** Prints the message with the given position indication. */
  def printMessageAndPos(msg: String, pos: SourcePosition, kind: String = "")(implicit ctx: Context): Unit = {
    val posStr = if (pos.exists) s"$pos: " else ""
    printMessage(s"${posStr}$kind: $msg")
    if (pos.exists) {
      printSourceLine(pos)
      printColumnMarker(pos)
    }
  }

  def printExplanation(m: Message)(implicit ctx: Context): Unit =
    printMessage(
      s"""|
          |Explanation
          |===========
          |${m.explanation}""".stripMargin
    )

  override def doReport(m: Message)(implicit ctx: Context): Unit = {
    m match {
      case m: Error =>
        printMessageAndPos(m.message, m.pos, m.kind)
        if (ctx.settings.prompt.value) displayPrompt()
      case m: ConditionalWarning if !m.enablingOption.value =>
      case m: MigrationWarning =>
        printMessageAndPos(m.message, m.pos, m.kind)
      case m: Warning =>
        printMessageAndPos(m.message, m.pos, m.kind)
      case _ =>
        printMessageAndPos(m.message, m.pos, m.kind)
    }

    if (ctx.shouldExplain(m)) printExplanation(m)
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
