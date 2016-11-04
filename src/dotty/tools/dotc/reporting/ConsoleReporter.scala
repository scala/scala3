package dotty.tools
package dotc
package reporting

import util.SourcePosition
import core.Contexts._
import java.io.{ BufferedReader, PrintWriter }
import diagnostic.{ Message, MessageContainer }
import diagnostic.messages._

/**
  * This class implements a Reporter that displays messages on a text console
  */
class ConsoleReporter(
  reader: BufferedReader = Console.in,
  writer: PrintWriter = new PrintWriter(Console.err, true)
) extends Reporter with UniqueMessagePositions with HideNonSensicalMessages with MessageRendering {

  import MessageContainer._

  /** maximal number of error messages to be printed */
  protected def ErrorLimit = 100

  /** Prints the message. */
  def printMessage(msg: String): Unit = { writer.print(msg + "\n"); writer.flush() }

  /** Prints the message with the given position indication. */
  def printMessageAndPos(msg: Message, pos: SourcePosition, diagnosticLevel: String)(implicit ctx: Context): Boolean = {
    printMessage(messageAndPos(msg, pos, diagnosticLevel))
    true
  }

  def printExplanation(m: Message)(implicit ctx: Context): Unit = {
    printMessage(explanation(m))
  }

  override def doReport(m: MessageContainer)(implicit ctx: Context): Unit = {
    val didPrint = m match {
      case m: Error =>
        val didPrint = printMessageAndPos(m.contained, m.pos, diagnosticLevel(m))
        if (ctx.settings.prompt.value) displayPrompt()
        didPrint
      case m: ConditionalWarning if !m.enablingOption.value =>
        false
      case m =>
        printMessageAndPos(m.contained, m.pos, diagnosticLevel(m))
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

