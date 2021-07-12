package dotty.tools
package dotc
package reporting

import scala.annotation.internal.sharable

import core.Contexts._
import core.Decorators._
import collection.mutable
import core.Mode
import dotty.tools.dotc.core.Symbols.{Symbol, NoSymbol}
import Diagnostic._
import ast.{tpd, Trees}
import Message._
import core.Decorators._
import util.NoSourcePosition

import java.io.{ BufferedReader, PrintWriter }

object Reporter {
  /** Convert a SimpleReporter into a real Reporter */
  def fromSimpleReporter(simple: interfaces.SimpleReporter): Reporter =
    new Reporter with UniqueMessagePositions with HideNonSensicalMessages {
      override def doReport(dia: Diagnostic)(using Context): Unit = simple.report(dia)
    }

  /** A reporter that ignores reports, and doesn't record errors */
  @sharable object NoReporter extends Reporter {
    def doReport(dia: Diagnostic)(using Context): Unit = ()
    override def report(dia: Diagnostic)(using Context): Unit = ()
  }

  type ErrorHandler = (Diagnostic, Context) => Unit

  private val defaultIncompleteHandler: ErrorHandler =
    (mc, ctx) => ctx.reporter.report(mc)(using ctx)

  /** Show prompt if `-Xprompt` is passed as a flag to the compiler */
  def displayPrompt(reader: BufferedReader, writer: PrintWriter): Unit = {
    writer.println()
    writer.print("a)bort, s)tack, r)esume: ")
    writer.flush()
    if (reader != null) {
      def loop(): Unit = reader.read match {
        case 'a' | 'A' =>
          new Throwable().printStackTrace(writer)
          System.exit(1)
        case 's' | 'S' =>
          new Throwable().printStackTrace(writer)
          writer.println()
          writer.flush()
        case 'r' | 'R' =>
          ()
        case _ =>
          loop()
      }
      loop()
    }
  }
}

/**
 * This interface provides methods to issue information, warning and
 * error messages.
 */
abstract class Reporter extends interfaces.ReporterResult {
  import Reporter._

  /** Report a diagnostic */
  def doReport(dia: Diagnostic)(using Context): Unit

  /** Whether very long lines can be truncated.  This exists so important
   *  debugging information (like printing the classpath) is not rendered
   *  invisible due to the max message length.
   */
  private var _truncationOK: Boolean = true
  def truncationOK: Boolean = _truncationOK
  def withoutTruncating[T](body: => T): T = {
    val saved = _truncationOK
    _truncationOK = false
    try body
    finally _truncationOK = saved
  }

  private var incompleteHandler: ErrorHandler = defaultIncompleteHandler

  def withIncompleteHandler[T](handler: ErrorHandler)(op: => T): T = {
    val saved = incompleteHandler
    incompleteHandler = handler
    try op
    finally incompleteHandler = saved
  }

  private var _errorCount = 0
  private var _warningCount = 0

  /** The number of errors reported by this reporter (ignoring outer reporters) */
  def errorCount: Int = _errorCount

  /** The number of warnings reported by this reporter (ignoring outer reporters) */
  def warningCount: Int = _warningCount

  /** Have errors been reported by this reporter (ignoring outer reporters)? */
  def hasErrors: Boolean = errorCount > 0

  /** Have warnings been reported by this reporter (ignoring outer reporters)? */
  def hasWarnings: Boolean = warningCount > 0

  private var errors: List[Error] = Nil

  /** All errors reported by this reporter (ignoring outer reporters) */
  def allErrors: List[Error] = errors

  /** Were sticky errors reported? Overridden in StoreReporter. */
  def hasStickyErrors: Boolean = false

  /** Have errors been reported by this reporter, or in the
   *  case where this is a StoreReporter, by an outer reporter?
   */
  def errorsReported: Boolean = hasErrors

  /** Run `op` and return `true` if errors were reported by this reporter.
   */
  def reportsErrorsFor(op: Context ?=> Unit)(using Context): Boolean = {
    val initial = errorCount
    op
    errorCount > initial
  }

  private var reportedFeaturesUseSites = Set[Symbol]()

  def isReportedFeatureUseSite(featureTrait: Symbol): Boolean =
    featureTrait.ne(NoSymbol) && reportedFeaturesUseSites.contains(featureTrait)

  def reportNewFeatureUseSite(featureTrait: Symbol): Unit = reportedFeaturesUseSites += featureTrait

  var unreportedWarnings: Map[String, Int] = Map.empty

  def issueIfNotSuppressed(dia: Diagnostic)(using Context): Unit =
    def go() =
      import Action._

      val toReport = dia match
        case w: Warning =>
          if ctx.settings.silentWarnings.value then None
          else if ctx.settings.XfatalWarnings.value && !w.isSummarizedConditional then Some(w.toError)
          else WConf.parsed.action(dia) match
            case Silent  => None
            case Info    => Some(w.toInfo)
            case Warning => Some(w)
            case Error   => Some(w.toError)
        case _ => Some(dia)

      toReport foreach {
        case cw: ConditionalWarning if cw.isSummarizedConditional =>
          val key = cw.enablingOption.name
          unreportedWarnings =
            unreportedWarnings.updated(key, unreportedWarnings.getOrElse(key, 0) + 1)
        case d if !isHidden(d) =>
          withMode(Mode.Printing)(doReport(d))
          d match {
            case _: Warning => _warningCount += 1
            case e: Error =>
              errors = e :: errors
              _errorCount += 1
              if ctx.typerState.isGlobalCommittable then
                ctx.base.errorsToBeReported = true
            case _: Info => // nothing to do here
            // match error if d is something else
          }
        case _ => // hidden
      }
    end go

    dia match
      case w: Warning if ctx.run != null =>
        val sup = ctx.run.suppressions
        if sup.suppressionsComplete(w.pos.source) then
          if !sup.isSuppressed(w) then go()
        else
          sup.addSuspendedMessage(w)
      case _ => go()
  end issueIfNotSuppressed

  def report(dia: Diagnostic)(using Context): Unit = issueIfNotSuppressed(dia)

  def incomplete(dia: Diagnostic)(using Context): Unit =
    incompleteHandler(dia, ctx)

  /** Summary of warnings and errors */
  def summary: String = {
    val b = new mutable.ListBuffer[String]
    if (warningCount > 0)
      b += countString(warningCount, "warning") + " found"
    if (errorCount > 0)
      b += countString(errorCount, "error") + " found"
    for ((settingName, count) <- unreportedWarnings)
      b += s"there were $count ${settingName.tail} warning(s); re-run with $settingName for details"
    b.mkString("\n")
  }

  /** Print the summary of warnings and errors */
  def printSummary(using Context): Unit = {
    val s = summary
    if (s != "") report(new Info(s, NoSourcePosition))
  }

  /** Returns a string meaning "n elements". */
  protected def countString(n: Int, elements: String): String = n match {
    case 0 => s"no ${elements}s"
    case 1 => s"1 ${elements}"
    case _ => s"$n ${elements}s"
  }

  /** Should this diagnostic not be reported at all? */
  def isHidden(dia: Diagnostic)(using Context): Boolean =
    ctx.mode.is(Mode.Printing)

  /** Does this reporter contain errors that have yet to be reported by its outer reporter ?
   *  Note: this is always false when there is no outer reporter.
   */
  def hasUnreportedErrors: Boolean = false

  /** If this reporter buffers messages, remove and return all buffered messages. */
  def removeBufferedMessages(using Context): List[Diagnostic] = Nil

  /** Issue all error messages in this reporter to next outer one, or make sure they are written. */
  def flush()(using Context): Unit =
    val msgs = removeBufferedMessages
    if msgs.nonEmpty then msgs.foreach(ctx.reporter.report)

  /** If this reporter buffers messages, all buffered messages, otherwise Nil */
  def pendingMessages(using Context): List[Diagnostic] = Nil
}
