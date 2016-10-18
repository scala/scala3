package dotty.tools
package dotc
package reporting

import scala.collection.mutable
import util.SourcePosition
import core.Contexts._
import Reporter._
import java.io.{ BufferedReader, IOException, PrintWriter }
import scala.reflect.internal.util._
import diagnostic.{ Message, MessageContainer, NoExplanation }
import diagnostic.messages._

/**
 * This class implements a Reporter that displays messages on a text
 * console.
 */
class ClassicReporter(
    reader: BufferedReader = Console.in,
    writer: PrintWriter = new PrintWriter(Console.err, true))
  extends Reporter with UniqueMessagePositions with HideNonSensicalMessages {

  import MessageContainer._

  /** maximal number of error messages to be printed */
  protected def ErrorLimit = 100

  def printPos(pos: SourcePosition): Unit =
    if (pos.exists) {
      printMessage(pos.lineContent.stripLineEnd)
      printMessage(" " * pos.column + "^")
      if (pos.outer.exists) {
        printMessage(s"\n... this location is in code that was inlined at ${pos.outer}:\n")
        printPos(pos.outer)
      }
    }

  /** Prints the message. */
  def printMessage(msg: String): Unit = { writer.print(msg + "\n"); writer.flush() }

  /** Prints the message with the given position indication. */
  def printMessageAndPos(msg: String, pos: SourcePosition)(implicit ctx: Context): Unit = {
    val posStr = if (pos.exists) s"$pos: " else ""
    printMessage(posStr + msg)
    printPos(pos)
  }

  override def doReport(m: MessageContainer)(implicit ctx: Context): Unit = m match {
    case m: Error =>
      printMessageAndPos(s"error: ${m.contained}", m.pos)
      if (ctx.settings.prompt.value) displayPrompt()
    case m: ConditionalWarning if !m.enablingOption.value =>
    case m: FeatureWarning =>
      printMessageAndPos(s"feature warning: ${m.contained.msg}", m.pos)
    case m: DeprecationWarning =>
      printMessageAndPos(s"deprecation warning: ${m.contained.msg}", m.pos)
    case m: UncheckedWarning =>
      printMessageAndPos(s"unchecked warning: ${m.contained.msg}", m.pos)
    case m: Info =>
      printMessageAndPos(m.contained.msg, m.pos)
    case m: MigrationWarning =>
      printMessageAndPos(s"migration warning: ${m.contained.msg}", m.pos)
    case m: Warning =>
      printMessageAndPos(s"warning: ${m.contained.msg}", m.pos)
    case _ =>
      printMessageAndPos(m.contained.msg, m.pos)
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
