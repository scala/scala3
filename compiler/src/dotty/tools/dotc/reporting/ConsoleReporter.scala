package dotty.tools
package dotc
package reporting

import core.Contexts.*
import java.io.{ BufferedReader, PrintWriter }
import Diagnostic.*
import dotty.tools.dotc.interfaces.Diagnostic.INFO

/**
  * This class implements a Reporter that displays messages on a text console
  */
class ConsoleReporter(
  reader: BufferedReader = Console.in,
  writer: PrintWriter = new PrintWriter(Console.err, true),
  echoer: PrintWriter = new PrintWriter(Console.out, true)
) extends ConsoleReporter.AbstractConsoleReporter {
  override def printMessage(msg: String): Unit = { writer.println(msg); writer.flush() }
  override def echoMessage(msg: String): Unit = { echoer.println(msg); echoer.flush() }
  override def flush()(using Context): Unit    = writer.flush()

  override def doReport(dia: Diagnostic)(using Context): Unit = {
    super.doReport(dia)
    if ctx.settings.Xprompt.value then
      dia match
        case _: Error                                        => Reporter.displayPrompt(reader, writer)
        case _: Warning if ctx.settings.XfatalWarnings.value => Reporter.displayPrompt(reader, writer)
        case _                                               =>
  }
}

object ConsoleReporter {
  abstract class AbstractConsoleReporter extends AbstractReporter {
    /** Print the diagnostic message. */
    def printMessage(msg: String): Unit

    /** Print the informative message. */
    def echoMessage(msg: String): Unit

    /** Print the message with the given position indication. */
    def doReport(dia: Diagnostic)(using Context): Unit =
      if dia.level == INFO then echoMessage(messageAndPos(dia))
      else printMessage(messageAndPos(dia))
  }
}
