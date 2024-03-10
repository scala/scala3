package dotty.tools
package dotc
package reporting

import core.Contexts.*
import java.io.{ BufferedReader, PrintWriter }
import Diagnostic.*

/**
  * This class implements a Reporter that displays messages on a text console
  */
class ConsoleReporter(
  reader: BufferedReader = Console.in,
  writer: PrintWriter = new PrintWriter(Console.err, true)
) extends ConsoleReporter.AbstractConsoleReporter {
  override def printMessage(msg: String): Unit = { writer.print(msg + "\n"); writer.flush() }
  override def flush()(using Context): Unit    = writer.flush()

  override def doReport(dia: Diagnostic)(using Context): Unit = {
    super.doReport(dia)
    if ctx.settings.Xprompt.value then
      dia match
        case _: Error                                        => Reporter.displayPrompt(reader, writer)
        case _: Warning if ctx.settings.XfatalWarnings.value => Reporter.displayPrompt(reader, writer)
        case _                                                 =>
  }
}

object ConsoleReporter {
  abstract class AbstractConsoleReporter extends AbstractReporter {
    /** Prints the message. */
    def printMessage(msg: String): Unit

    /** Prints the message with the given position indication. */
    def doReport(dia: Diagnostic)(using Context): Unit = {
      printMessage(messageAndPos(dia))
    }
  }
}
