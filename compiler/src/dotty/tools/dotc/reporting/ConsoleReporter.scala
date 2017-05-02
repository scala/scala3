package dotty.tools
package dotc
package reporting

import core.Contexts._
import java.io.{ BufferedReader, PrintWriter }
import diagnostic.{ Message, MessageContainer }
import diagnostic.messages.{ Error, Warning, ConditionalWarning }

/**
  * This class implements a Reporter that displays messages on a text console
  */
class ConsoleReporter(
  reader: BufferedReader = Console.in,
  writer: PrintWriter = new PrintWriter(Console.err, true)
) extends Reporter with UniqueMessagePositions with HideNonSensicalMessages with MessageRendering {

  import MessageContainer._

  /** Prints the message. */
  def printMessage(msg: String): Unit = { writer.print(msg + "\n"); writer.flush() }

  /** Prints the message with the given position indication. */
  def doReport(m: MessageContainer)(implicit ctx: Context): Unit = {
    val didPrint = m match {
      case m: Error =>
        printMessage(messageAndPos(m.contained(), m.pos, diagnosticLevel(m)))
        if (ctx.settings.prompt.value) displayPrompt()
        true
      case m: ConditionalWarning if !m.enablingOption.value =>
        false
      case m =>
        printMessage(messageAndPos(m.contained(), m.pos, diagnosticLevel(m)))
        true
    }

    if (didPrint && ctx.shouldExplain(m))
      printMessage(explanation(m.contained()))
    else if (didPrint && m.contained().explanation.nonEmpty)
      printMessage("\nlonger explanation available when compiling with `-explain`")
  }

  /** Show prompt if `-Xprompt` is passed as a flag to the compiler */
  def displayPrompt()(implicit ctx: Context): Unit = {
    printMessage("\na)bort, s)tack, r)esume: ")
    flush()
    if (reader != null) {
      val response = reader.read().asInstanceOf[Char].toLower
      if (response == 'a' || response == 's') {
        Thread.dumpStack()
        if (response == 'a')
          sys.exit(1)
      }
      print("\n")
      flush()
    }
  }

  override def flush()(implicit ctx: Context): Unit = { writer.flush() }
}
