package dotty.tools
package dotc
package reporting

import scala.annotation.internal.sharable

import core.Contexts._
import core.TypeError
import util.{SourcePosition, NoSourcePosition}
import core.Decorators.PhaseListDecorator
import collection.mutable
import core.Mode
import dotty.tools.dotc.core.Symbols.{Symbol, NoSymbol}
import Diagnostic._
import ast.{tpd, Trees}
import Message._
import core.Decorators._
import config.Feature.sourceVersion
import config.SourceVersion

import java.lang.System.currentTimeMillis
import java.io.{ BufferedReader, PrintWriter }


object Reporter {
  /** Convert a SimpleReporter into a real Reporter */
  def fromSimpleReporter(simple: interfaces.SimpleReporter): Reporter =
    new Reporter with UniqueMessagePositions with HideNonSensicalMessages {
      override def doReport(dia: Diagnostic)(using Context): Unit = dia match {
        case dia: ConditionalWarning if !dia.enablingOption.value =>
        case _ =>
          simple.report(dia)
      }
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

trait Reporting { thisCtx: Context =>

  /** For sending messages that are printed only if -verbose is set */
  def inform(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    if (thisCtx.settings.verbose.value) thisCtx.echo(msg, pos)

  def echo(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(new Info(msg, pos))

  def reportWarning(warning: Warning): Unit =
    if (!thisCtx.settings.silentWarnings.value)
      if (thisCtx.settings.XfatalWarnings.value)
        warning match {
          case warning: ConditionalWarning if !warning.enablingOption.value =>
            reporter.report(warning) // conditional warnings that are not enabled are not fatal
          case _ =>
            reporter.report(warning.toError)
        }
      else reporter.report(warning)

  def deprecationWarning(msg: Message, pos: SourcePosition = NoSourcePosition): Unit =
    reportWarning(new DeprecationWarning(msg, pos))

  def migrationWarning(msg: Message, pos: SourcePosition = NoSourcePosition): Unit =
    reportWarning(new MigrationWarning(msg, pos))

  def uncheckedWarning(msg: Message, pos: SourcePosition = NoSourcePosition): Unit =
    reportWarning(new UncheckedWarning(msg, pos))

  def featureWarning(msg: Message, pos: SourcePosition = NoSourcePosition): Unit =
    reportWarning(new FeatureWarning(msg, pos))

  def featureWarning(feature: String, featureDescription: String,
      featureUseSite: Symbol, required: Boolean, pos: SourcePosition): Unit = {
    val req = if (required) "needs to" else "should"
    val fqname = s"scala.language.$feature"

    val explain =
      if (reporter.isReportedFeatureUseSite(featureUseSite)) ""
      else {
        reporter.reportNewFeatureUseSite(featureUseSite)
        s"""
           |See the Scala docs for value $fqname for a discussion
           |why the feature $req be explicitly enabled.""".stripMargin
      }

    val msg = s"""$featureDescription $req be enabled
                 |by adding the import clause 'import $fqname'
                 |or by setting the compiler option -language:$feature.$explain""".stripMargin
    if (required) error(msg, pos)
    else reportWarning(new FeatureWarning(msg, pos))
  }

  def warning(msg: Message, pos: SourcePosition = NoSourcePosition): Unit =
    reportWarning(new Warning(msg, addInlineds(pos)))

  def error(msg: Message, pos: SourcePosition = NoSourcePosition, sticky: Boolean = false): Unit = {
    val fullPos = addInlineds(pos)
    reporter.report(if (sticky) new StickyError(msg, fullPos) else new Error(msg, fullPos))
    if thisCtx.settings.YdebugError.value then Thread.dumpStack()
  }

  def error(ex: TypeError, pos: SourcePosition): Unit = {
    error(ex.toMessage, pos, sticky = true)
    if (thisCtx.settings.YdebugTypeError.value)
      ex.printStackTrace()
  }

  def errorOrMigrationWarning(msg: Message, pos: SourcePosition = NoSourcePosition,
      from: SourceVersion = SourceVersion.defaultSourceVersion): Unit =
    if sourceVersion.isAtLeast(from) then
      if sourceVersion.isMigrating then migrationWarning(msg, pos)
      else error(msg, pos)

  def restrictionError(msg: Message, pos: SourcePosition = NoSourcePosition): Unit =
    error(msg.mapMsg("Implementation restriction: " + _), pos)

  def incompleteInputError(msg: Message, pos: SourcePosition = NoSourcePosition)(using Context): Unit =
    reporter.incomplete(new Error(msg, pos))

  /** Log msg if settings.log contains the current phase.
   *  See [[config.CompilerCommand#explainAdvanced]] for the exact meaning of
   *  "contains" here.
   */
  def log(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    if (thisCtx.settings.Ylog.value.containsPhase(phase))
      echo(s"[log ${thisCtx.phasesStack.reverse.mkString(" -> ")}] $msg", pos)

  def debuglog(msg: => String): Unit =
    if (thisCtx.debug) log(msg)

  def informTime(msg: => String, start: Long): Unit = {
    def elapsed = s" in ${currentTimeMillis - start}ms"
    informProgress(msg + elapsed)
  }

  def informProgress(msg: => String): Unit =
    inform("[" + msg + "]")

  def logWith[T](msg: => String)(value: T): T = {
    log(msg + " " + value)
    value
  }

  def debugwarn(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    if (thisCtx.settings.Ydebug.value) warning(msg, pos)

  private def addInlineds(pos: SourcePosition)(using Context) = {
    def recur(pos: SourcePosition, inlineds: List[Trees.Tree[?]]): SourcePosition = inlineds match {
      case inlined :: inlineds1 => pos.withOuter(recur(inlined.sourcePos, inlineds1))
      case Nil => pos
    }
    recur(pos, tpd.enclosingInlineds)
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
  def reportsErrorsFor(op: Context => Unit)(using Context): Boolean = {
    val initial = errorCount
    op(ctx)
    errorCount > initial
  }

  private var reportedFeaturesUseSites = Set[Symbol]()

  def isReportedFeatureUseSite(featureTrait: Symbol): Boolean =
    featureTrait.ne(NoSymbol) && reportedFeaturesUseSites.contains(featureTrait)

  def reportNewFeatureUseSite(featureTrait: Symbol): Unit = reportedFeaturesUseSites += featureTrait

  var unreportedWarnings: Map[String, Int] = Map.empty

  def report(dia: Diagnostic)(using Context): Unit =
    if (!isHidden(dia)) {
      doReport(dia)(using ctx.addMode(Mode.Printing))
      dia match {
        case dia: ConditionalWarning if !dia.enablingOption.value =>
          val key = dia.enablingOption.name
          unreportedWarnings =
            unreportedWarnings.updated(key, unreportedWarnings.getOrElse(key, 0) + 1)
        case dia: Warning => _warningCount += 1
        case dia: Error =>
          errors = dia :: errors
          _errorCount += 1
        case dia: Info => // nothing to do here
        // match error if d is something else
      }
    }

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
    if (s != "") ctx.echo(s)
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
    removeBufferedMessages.foreach(ctx.reporter.report)

  /** If this reporter buffers messages, all buffered messages, otherwise Nil */
  def pendingMessages(using Context): List[Diagnostic] = Nil
}
