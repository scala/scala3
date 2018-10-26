package dotty.tools
package dotc
package reporting

import scala.annotation.internal.sharable

import core.Contexts._
import util.{SourcePosition, NoSourcePosition}
import core.Decorators.PhaseListDecorator
import collection.mutable
import java.lang.System.currentTimeMillis
import core.Mode
import dotty.tools.dotc.core.Symbols.{Symbol, NoSymbol}
import diagnostic.messages._
import diagnostic._
import Message._

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
}


trait Reporting { this: Context =>

import dotty.tools.dotc.reporting.diagnostic.MessageContainer

  type Escalation = MessageContainer => Option[MessageContainer]
  val suppression: Escalation = (mc: MessageContainer) => for {
    nonSilent <- silentWarnings(mc)
    allowedId <- suppressIds(nonSilent)
    allowedKind <- suppressKinds(allowedId)
    escalated <- fatalWarnings(allowedKind)
  } yield escalated

  def suppressedIds: List[Int] = this.settings.XsuppressMessageIds.value.flatMap(id => scala.util.Try(id.toInt).toOption)
  def suppressIds(mc: MessageContainer) = mc match {
    case w: Warning if suppressedIds.contains(w.contained().errorId) => None
    case _ => Some(mc)
  }

  def supressedWarningKinds: List[String] = this.settings.XsuppressWarningKinds.value
  def suppressKinds(mc: MessageContainer) = mc match {
    case w: Warning if supressedWarningKinds.contains(w.contained().kind) => None
    case _ => Some(mc)
  }

  val silentWarnings: Escalation = (mc: MessageContainer) => mc match {
    case _: Warning if this.settings.silentWarnings.value(this) => None
    case _ => Some(mc)
  }

  val fatalWarnings: Escalation = (mc: MessageContainer) => mc match {
    case w: Warning if this.settings.XfatalWarnings.value(this) => Some(w.toError)
    case _ => Some(mc)
  }


  def report(cont: MessageContainer) = suppression(cont) foreach reporter.report _

  /** For sending messages that are printed only if -verbose is set */
  def inform(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    if (this.settings.verbose.value) this.echo(msg, pos)

  def echo(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    report(new Info(msg, pos))

  def deprecationWarning(msg: => Message, pos: SourcePosition = NoSourcePosition): Unit =
    report(new DeprecationWarning(msg, pos))

  def migrationWarning(msg: => Message, pos: SourcePosition = NoSourcePosition): Unit =
    report(new MigrationWarning(msg, pos))

  def uncheckedWarning(msg: => Message, pos: SourcePosition = NoSourcePosition): Unit =
    report(new UncheckedWarning(msg, pos))

  def featureWarning(msg: => Message, pos: SourcePosition = NoSourcePosition): Unit =
    report(new FeatureWarning(msg, pos))

  def featureWarning(feature: String, featureDescription: String, isScala2Feature: Boolean,
      featureUseSite: Symbol, required: Boolean, pos: SourcePosition): Unit = {
    val req = if (required) "needs to" else "should"
    val prefix = if (isScala2Feature) "scala." else "dotty."
    val fqname = prefix + "language." + feature

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
    else report(new FeatureWarning(msg, pos))
  }

  def warning(msg: => Message, pos: SourcePosition = NoSourcePosition): Unit =
    report(new Warning(msg, pos))


  def strictWarning(msg: => Message, pos: SourcePosition = NoSourcePosition): Unit =
    if (this.settings.strict.value) error(msg, pos)
    else report(new ExtendMessage(() => msg)(_ + "\n(This would be an error under strict mode)").warning(pos))

  def error(msg: => Message, pos: SourcePosition = NoSourcePosition): Unit =
    report(new Error(msg, pos))

  def errorOrMigrationWarning(msg: => Message, pos: SourcePosition = NoSourcePosition): Unit =
    if (ctx.scala2Mode) migrationWarning(msg, pos) else error(msg, pos)

  def restrictionError(msg: => Message, pos: SourcePosition = NoSourcePosition): Unit =
    report {
      new ExtendMessage(() => msg)(m => s"Implementation restriction: $m").error(pos)
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
}

/**
 * This interface provides methods to issue information, warning and
 * error messages.
 */
abstract class Reporter extends interfaces.ReporterResult {

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

  type ErrorHandler = MessageContainer => Context => Unit
  private[this] var incompleteHandler: ErrorHandler = d => c => report(d)(c)
  def withIncompleteHandler[T](handler: ErrorHandler)(op: => T): T = {
    val saved = incompleteHandler
    incompleteHandler = handler
    try op
    finally incompleteHandler = saved
  }

  private[this] var _errorCount = 0
  private[this] var _warningCount = 0
  def errorCount: Int = _errorCount
  def warningCount: Int = _warningCount
  def hasErrors: Boolean = errorCount > 0
  def hasWarnings: Boolean = warningCount > 0
  private[this] var errors: List[Error] = Nil
  def allErrors: List[Error] = errors

  /** Have errors been reported by this reporter, or in the
   *  case where this is a StoreReporter, by an outer reporter?
   */
  def errorsReported: Boolean = hasErrors

  private[this] var reportedFeaturesUseSites = Set[Symbol]()

  def isReportedFeatureUseSite(featureTrait: Symbol): Boolean =
    featureTrait.ne(NoSymbol) && reportedFeaturesUseSites.contains(featureTrait)

  def reportNewFeatureUseSite(featureTrait: Symbol): Unit = reportedFeaturesUseSites += featureTrait

  val unreportedWarnings: mutable.HashMap[String, Int] = new mutable.HashMap[String, Int] {
    override def default(key: String) = 0
  }

  def report(m: MessageContainer)(implicit ctx: Context): Unit =
    if (!isHidden(m)) {
      doReport(m)(ctx.addMode(Mode.Printing))
      m match {
        case m: ConditionalWarning if !m.enablingOption.value => unreportedWarnings(m.enablingOption.name) += 1
        case m: Warning => _warningCount += 1
        case m: Error =>
          errors = m :: errors
          _errorCount += 1
        case m: Info => // nothing to do here
        // match error if d is something else
      }
    }

  def incomplete(m: MessageContainer)(implicit ctx: Context): Unit =
    incompleteHandler(m)(ctx)

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

  /** Does this reporter contain not yet reported errors or warnings? */
  def hasPendingErrors: Boolean = false

  /** If this reporter buffers messages, remove and return all buffered messages. */
  def removeBufferedMessages(implicit ctx: Context): List[MessageContainer] = Nil

  /** Issue all error messages in this reporter to next outer one, or make sure they are written. */
  def flush()(implicit ctx: Context): Unit =
    removeBufferedMessages.foreach(ctx.reporter.report)
}
