package dotty.tools
package dotc
package reporting

import core.Contexts._
import java.io.{ BufferedReader, PrintWriter }
import diagnostic.MessageContainer
import diagnostic.messages.{ Error, ConditionalWarning }

/**
  * This class implements a Reporter that displays messages on a text console
  */
class ConsoleReporter(
  reader: BufferedReader = Console.in,
  writer: PrintWriter = new PrintWriter(Console.err, true)
) extends AbstractReporter {

  import MessageContainer._

  /** Prints the message. */
  def printMessage(msg: String): Unit = { writer.print(msg + "\n"); writer.flush() }

  /** Prints the message with the given position indication. */
  def doReport(m: MessageContainer)(implicit ctx: Context): Unit = {
    val didPrint = m match {
      case m: Error =>
        printMessage(messageAndPos(m.contained(), m.pos, diagnosticLevel(m)))
        if (ctx.settings.Xprompt.value) Reporter.displayPrompt(reader, writer)
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

  override def flush()(implicit ctx: Context): Unit = { writer.flush() }
}
