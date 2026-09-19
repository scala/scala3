package dotty.tools
package dotc
package reporting

import core.Contexts.*
import java.io.{ BufferedReader, PrintWriter }
import Diagnostic.*
import dotty.tools.dotc.interfaces.Diagnostic.INFO
import scala.annotation.threadUnsafe

/**
  * This class implements a Reporter that displays messages on a text console
  */
class ConsoleReporter(
  reader: BufferedReader | Null = Console.in,
  initWriter: PrintWriter | Null = null,
  initEchoer: PrintWriter | Null = null
) extends ConsoleReporter.AbstractConsoleReporter {
  // Avoid allocating these if we don't need them, i.e., if compilation succeeds without diagnostics.
  @threadUnsafe
  private lazy val writer: PrintWriter = if initWriter != null then initWriter else new PrintWriter(Console.err, true)
  @threadUnsafe
  private lazy val echoer: PrintWriter = if initEchoer != null then initEchoer else new PrintWriter(Console.out, true)

  override def printMessage(msg: String): Unit = { writer.println(msg); writer.flush() }
  override def echoMessage(msg: String): Unit = { echoer.println(msg); echoer.flush() }
  override def flush()(using Context): Unit    = writer.flush()

  override def doReport(dia: Diagnostic)(using Context): Unit = {
    super.doReport(dia)
    if ctx.settings.Xprompt.value then
      dia match
        case _: Error   => Reporter.displayPrompt(reader, writer)
        case _: Warning => if ctx.settings.Werror.value then Reporter.displayPrompt(reader, writer)
        case _          =>
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
