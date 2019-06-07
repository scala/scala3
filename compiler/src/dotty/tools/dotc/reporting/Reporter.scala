package dotty.tools
package dotc
package reporting

import scala.annotation.internal.sharable

import core.Contexts._
import util.{SourcePosition, NoSourcePosition}
import core.Decorators.PhaseListDecorator
import collection.mutable
import core.Mode
import dotty.tools.dotc.core.Symbols.{Symbol, NoSymbol}
import diagnostic.messages._
import diagnostic._
import ast.{tpd, Trees}
import Message._

import java.lang.System.currentTimeMillis
import java.io.{ BufferedReader, PrintWriter }


object Reporter {
  /** Convert a SimpleReporter into a real Reporter */
  def fromSimpleReporter(simple: interfaces.SimpleReporter): Reporter =
    new Reporter with UniqueMessagePositions with HideNonSensicalMessages {
      override def doReport(m: MessageContainer)(implicit ctx: Context): Unit = m match {
        case m: ConditionalWarning if !m.enablingOption.value =>
        case _ =>
          simple.report(m)
      }
    }

  /** A reporter that ignores reports, and doesn't record errors */
  @sharable object NoReporter extends Reporter {
    def doReport(m: MessageContainer)(implicit ctx: Context): Unit = ()
    override def report(m: MessageContainer)(implicit ctx: Context): Unit = ()
  }

  type ErrorHandler = (MessageContainer, Context) => Unit

  private val defaultIncompleteHandler: ErrorHandler =
    (mc, ctx) => ctx.reporter.report(mc)(ctx)

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

trait Reporting { this: Context =>

  /** For sending messages that are printed only if -verbose is set */
  def inform(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    if (this.settings.verbose.value) this.echo(msg, pos)

  def echo(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(new Info(msg, pos))

  def reportWarning(warning: Warning): Unit =
    if (!this.settings.silentWarnings.value) {
      if (this.settings.XfatalWarnings.value)
        warning match {
          case warning: ConditionalWarning if !warning.enablingOption.value =>
            reporter.report(warning) // conditional warnings that are not enabled are not fatal
          case _ =>
            reporter.report(warning.toError)
        }
      else reporter.report(warning)
    }

  def deprecationWarning(msg: => Message, pos: SourcePosition = NoSourcePosition): Unit =
    reportWarning(new DeprecationWarning(msg, pos))

  def migrationWarning(msg: => Message, pos: SourcePosition = NoSourcePosition): Unit =
    reportWarning(new MigrationWarning(msg, pos))

  def uncheckedWarning(msg: => Message, pos: SourcePosition = NoSourcePosition): Unit =
    reportWarning(new UncheckedWarning(msg, pos))

  def featureWarning(msg: => Message, pos: SourcePosition = NoSourcePosition): Unit =
    reportWarning(new FeatureWarning(msg, pos))

  def featureWarning(feature: String, featureDescription: String,
      featureUseSite: Symbol, required: Boolean, pos: SourcePosition): Unit = {
    val req = if (required) "needs to" else "should"
    val fqname = s"scala.language.$feature"

    val explain = {
      if (reporter.isReportedFeatureUseSite(featureUseSite)) ""
      else {
        reporter.reportNewFeatureUseSite(featureUseSite)
        s"""
           |This can be achieved by adding the import clause 'import $fqname'
           |or by setting the compiler option -language:$feature.
           |See the Scala docs for value $fqname for a discussion
           |why the feature $req be explicitly enabled.""".stripMargin
      }
    }

    val msg = s"$featureDescription $req be enabled\nby making the implicit value $fqname visible.$explain"
    if (required) error(msg, pos)
    else reportWarning(new FeatureWarning(msg, pos))
  }

  def warning(msg: => Message, pos: SourcePosition = NoSourcePosition): Unit =
    reportWarning(new Warning(msg, addInlineds(pos)))

  def strictWarning(msg: => Message, pos: SourcePosition = NoSourcePosition): Unit = {
    val fullPos = addInlineds(pos)
    if (this.settings.strict.value) error(msg, fullPos)
    else reportWarning(
      new ExtendMessage(() => msg)(_ + "\n(This would be an error under strict mode)")
        .warning(fullPos))
  }

  def error(msg: => Message, pos: SourcePosition = NoSourcePosition, sticky: Boolean = false): Unit = {
    val fullPos = addInlineds(pos)
    reporter.report(if (sticky) new StickyError(msg, fullPos) else new Error(msg, fullPos))
  }

  def errorOrMigrationWarning(msg: => Message, pos: SourcePosition = NoSourcePosition): Unit =
    if (ctx.scala2Mode) migrationWarning(msg, pos) else error(msg, pos)

  def restrictionError(msg: => Message, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report {
      new ExtendMessage(() => msg)(m => s"Implementation restriction: $m").error(addInlineds(pos))
    }

  def incompleteInputError(msg: => Message, pos: SourcePosition = NoSourcePosition)(implicit ctx: Context): Unit =
    reporter.incomplete(new Error(msg, pos))(ctx)

  /** Log msg if settings.log contains the current phase.
   *  See [[config.CompilerCommand#explainAdvanced]] for the exact meaning of
   *  "contains" here.
   */
  def log(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    if (this.settings.Ylog.value.containsPhase(phase))
      echo(s"[log ${ctx.phasesStack.reverse.mkString(" -> ")}] $msg", pos)

  def debuglog(msg: => String): Unit =
    if (ctx.debug) log(msg)

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
    if (this.settings.Ydebug.value) warning(msg, pos)

  private def addInlineds(pos: SourcePosition)(implicit ctx: Context) = {
    def recur(pos: SourcePosition, inlineds: List[Trees.Tree[_]]): SourcePosition = inlineds match {
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
  def doReport(m: MessageContainer)(implicit ctx: Context): Unit

  /** Whether very long lines can be truncated.  This exists so important
   *  debugging information (like printing the classpath) is not rendered
   *  invisible due to the max message length.
   */
  private[this] var _truncationOK: Boolean = true
  def truncationOK: Boolean = _truncationOK
  def withoutTruncating[T](body: => T): T = {
    val saved = _truncationOK
    _truncationOK = false
    try body
    finally _truncationOK = saved
  }

  private[this] var incompleteHandler: ErrorHandler = defaultIncompleteHandler

  def withIncompleteHandler[T](handler: ErrorHandler)(op: => T): T = {
    val saved = incompleteHandler
    incompleteHandler = handler
    try op
    finally incompleteHandler = saved
  }

  private[this] var _errorCount = 0
  private[this] var _warningCount = 0

  /** The number of errors reported by this reporter (ignoring outer reporters) */
  def errorCount: Int = _errorCount

  /** The number of warnings reported by this reporter (ignoring outer reporters) */
  def warningCount: Int = _warningCount

  /** Have errors been reported by this reporter (ignoring outer reporters)? */
  def hasErrors: Boolean = errorCount > 0

  /** Have warnings been reported by this reporter (ignoring outer reporters)? */
  def hasWarnings: Boolean = warningCount > 0

  private[this] var errors: List[Error] = Nil

  /** All errors reported by this reporter (ignoring outer reporters) */
  def allErrors: List[Error] = errors

  /** Were sticky errors reported? Overridden in StoreReporter. */
  def hasStickyErrors: Boolean = false

  /** Have errors been reported by this reporter, or in the
   *  case where this is a StoreReporter, by an outer reporter?
   */
  def errorsReported: Boolean = hasErrors

  private[this] var reportedFeaturesUseSites = Set[Symbol]()

  def isReportedFeatureUseSite(featureTrait: Symbol): Boolean =
    featureTrait.ne(NoSymbol) && reportedFeaturesUseSites.contains(featureTrait)

  def reportNewFeatureUseSite(featureTrait: Symbol): Unit = reportedFeaturesUseSites += featureTrait

  var unreportedWarnings: Map[String, Int] = Map.empty

  def report(m: MessageContainer)(implicit ctx: Context): Unit =
    if (!isHidden(m)) {
      doReport(m)(ctx.addMode(Mode.Printing))
      m match {
        case m: ConditionalWarning if !m.enablingOption.value =>
          val key = m.enablingOption.name
          unreportedWarnings =
            unreportedWarnings.updated(key, unreportedWarnings.getOrElse(key, 0) + 1)
        case m: Warning => _warningCount += 1
        case m: Error =>
          errors = m :: errors
          _errorCount += 1
        case m: Info => // nothing to do here
        // match error if d is something else
      }
    }

  def incomplete(m: MessageContainer)(implicit ctx: Context): Unit =
    incompleteHandler(m, ctx)

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
  def printSummary(implicit ctx: Context): Unit = {
    val s = summary
    if (s != "") ctx.echo(s)
  }

  /** Returns a string meaning "n elements". */
  protected def countString(n: Int, elements: String): String = n match {
    case 0 => "no " + elements + "s"
    case 1 => "one " + elements
    case 2 => "two " + elements + "s"
    case 3 => "three " + elements + "s"
    case 4 => "four " + elements + "s"
    case _ => n + " " + elements + "s"
  }

  /** Should this diagnostic not be reported at all? */
  def isHidden(m: MessageContainer)(implicit ctx: Context): Boolean =
    ctx.mode.is(Mode.Printing)

  /** Does this reporter contain errors that have yet to be reported by its outer reporter ?
   *  Note: this is always false when there is no outer reporter.
   */
  def hasUnreportedErrors: Boolean = false

  /** If this reporter buffers messages, remove and return all buffered messages. */
  def removeBufferedMessages(implicit ctx: Context): List[MessageContainer] = Nil

  /** Issue all error messages in this reporter to next outer one, or make sure they are written. */
  def flush()(implicit ctx: Context): Unit =
    removeBufferedMessages.foreach(ctx.reporter.report)
}
