package dotty.tools
package dotc
package reporting

import core.Contexts.*
import collection.mutable
import config.Printers.typr
import Diagnostic.*

/** This class implements a Reporter that stores all messages
  *
  * Beware that this reporter can leak memory, and force messages in two
  * scenarios:
  *
  * - During debugging `config.Printers.typr` is set from `noPrinter` to `new
  *   Printer`, which forces the message
  * - The reporter is not flushed and the message containers capture a
  *   `Context` (about 4MB)
  */
class StoreReporter(outer: Reporter | Null = Reporter.NoReporter, fromTyperState: Boolean = false) extends Reporter {

  protected var infos: mutable.ListBuffer[Diagnostic] | Null = null

  override def doReport(dia: Diagnostic)(using Context): Unit = {
    typr.println(s">>>> StoredError: ${dia.message}") // !!! DEBUG
    val infos1 = initialize(infos, infos = _, new mutable.ListBuffer)
    infos1 += dia
  }

  override def hasUnreportedErrors: Boolean =
    outer != null && (infos match
      case null => false
      case is => is.exists(_.isInstanceOf[Error]))

  override def hasStickyErrors: Boolean =
    infos match
      case null => false
      case is => is.exists(_.isInstanceOf[StickyError])

  override def removeBufferedMessages(using Context): List[Diagnostic] =
    infos match
      case null => Nil
      case is => try is.toList finally infos = null

  override def mapBufferedMessages(f: Diagnostic => Diagnostic)(using Context): Unit =
    infos match
      case null => ()
      case is => is.mapInPlace(f)

  override def pendingMessages(using Context): List[Diagnostic] =
    infos match
      case null => Nil
      case is => is.toList

  override def errorsReported: Boolean = hasErrors || (outer != null && outer.errorsReported)

  // If this is a TyperState buffering reporter then buffer the messages,
  // so that then only when the messages are unbuffered (when the reporter if flushed)
  // do they go through -Wconf, and possibly then buffered on the Run as a suspended message
  override def report(dia: Diagnostic)(using Context): Unit =
    if fromTyperState then issueUnconfigured(dia, forced = false)
    else super.report(dia)
}
