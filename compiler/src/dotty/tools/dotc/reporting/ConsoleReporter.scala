package dotty.tools
package dotc
package reporting

import core.Contexts._
import java.io.{ BufferedReader, PrintWriter }
import Diagnostic.{ Error, ConditionalWarning }

/**
  * This class implements a Reporter that displays messages on a text console
  */
class ConsoleReporter(
  reader: BufferedReader = Console.in,
  writer: PrintWriter = new PrintWriter(Console.err, true)
) extends AbstractReporter {

  import Diagnostic._

  /** Prints the message. */
  def printMessage(msg: String): Unit = { writer.print(msg + "\n"); writer.flush() }

  /** Prints the message with the given position indication. */
  def doReport(dia: Diagnostic)(using Context): Unit = {
    val didPrint = dia match {
      case dia: Error =>
        printMessage(messageAndPos(dia.msg, dia.pos, diagnosticLevel(dia)))
        if (ctx.settings.Xprompt.value) Reporter.displayPrompt(reader, writer)
        true
      case dia: ConditionalWarning if !dia.enablingOption.value =>
        false
      case dia =>
        printMessage(messageAndPos(dia.msg, dia.pos, diagnosticLevel(dia)))
        true
    }

    if (didPrint && shouldExplain(dia))
      printMessage(explanation(dia.msg))
    else if (didPrint && dia.msg.canExplain)
      printMessage("\nlonger explanation available when compiling with `-explain`")
  }

  override def flush()(using Context): Unit = { writer.flush() }
}
