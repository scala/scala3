package dotty.tools
package dotc
package reporting

import core.Contexts._
import util.{SourcePosition, NoSourcePosition}
import util.{SourceFile, NoSource}
import core.Decorators.PhaseListDecorator
import collection.mutable
import config.Settings.Setting
import config.Printers
import java.lang.System.currentTimeMillis
import core.Mode
import interfaces.Diagnostic.{ERROR, WARNING, INFO}
import dotty.tools.dotc.core.Symbols.Symbol

object Reporter {
  class Error(msgFn: => String, pos: SourcePosition) extends Diagnostic(msgFn, pos, ERROR)
  class Warning(msgFn: => String, pos: SourcePosition) extends Diagnostic(msgFn, pos, WARNING)
  class Info(msgFn: => String, pos: SourcePosition) extends Diagnostic(msgFn, pos, INFO)

  abstract class ConditionalWarning(msgFn: => String, pos: SourcePosition) extends Warning(msgFn, pos) {
    def enablingOption(implicit ctx: Context): Setting[Boolean]
  }
  class FeatureWarning(msgFn: => String, pos: SourcePosition) extends ConditionalWarning(msgFn, pos) {
    def enablingOption(implicit ctx: Context) = ctx.settings.feature
  }
  class UncheckedWarning(msgFn: => String, pos: SourcePosition) extends ConditionalWarning(msgFn, pos) {
    def enablingOption(implicit ctx: Context) = ctx.settings.unchecked
  }
  class DeprecationWarning(msgFn: => String, pos: SourcePosition) extends ConditionalWarning(msgFn, pos) {
    def enablingOption(implicit ctx: Context) = ctx.settings.deprecation
  }
  class MigrationWarning(msgFn: => String, pos: SourcePosition) extends ConditionalWarning(msgFn, pos) {
    def enablingOption(implicit ctx: Context) = ctx.settings.migration
  }

  /** Convert a SimpleReporter into a real Reporter */
  def fromSimpleReporter(simple: interfaces.SimpleReporter): Reporter =
    new Reporter with UniqueMessagePositions with HideNonSensicalMessages {
      override def doReport(d: Diagnostic)(implicit ctx: Context): Unit = d match {
        case d: ConditionalWarning if !d.enablingOption.value =>
        case _ =>
          simple.report(d)
      }
    }
}

import Reporter._

trait Reporting { this: Context =>

  /** For sending messages that are printed only if -verbose is set */
  def inform(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    if (this.settings.verbose.value) this.echo(msg, pos)

  def echo(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(new Info(msg, pos))

  def deprecationWarning(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(new DeprecationWarning(msg, pos))

  def migrationWarning(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(new MigrationWarning(msg, pos))

  def uncheckedWarning(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(new UncheckedWarning(msg, pos))

  def featureWarning(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(new FeatureWarning(msg, pos))

  def featureWarning(feature: String, featureDescription: String, isScala2Feature: Boolean,
      featureUseSite: Symbol, required: Boolean, pos: SourcePosition): Unit = {
    val req = if (required) "needs to" else "should"
    val prefix = if (isScala2Feature) "scala." else "dotty."
    val fqname = prefix + "language." + feature

    val explain = {
      if (reporter.isReportedFeatureUseSite(featureUseSite)) ""
      else {
        reporter.reportNewFeatureUseSite(featureUseSite)
        s"""|
            |This can be achieved by adding the import clause 'import $fqname'
            |or by setting the compiler option -language:$feature.
            |See the Scala docs for value $fqname for a discussion
            |why the feature $req be explicitly enabled.""".stripMargin
      }
    }

    val msg = s"$featureDescription $req be enabled\nby making the implicit value $fqname visible.$explain"
    if (required) error(msg, pos)
    else reporter.report(new FeatureWarning(msg, pos))
  }

  def warning(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(new Warning(msg, pos))

  def strictWarning(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    if (this.settings.strict.value) error(msg, pos)
    else warning(msg + "\n(This would be an error under strict mode)", pos)

  def error(msg: => String, pos: SourcePosition = NoSourcePosition): Unit = {
    // println("*** ERROR: " + msg) // !!! DEBUG
    reporter.report(new Error(msg, pos))
  }

  def errorOrMigrationWarning(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    if (ctx.scala2Mode) migrationWarning(msg, pos) else error(msg, pos)

  def restrictionError(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    error(s"Implementation restriction: $msg", pos)

  def incompleteInputError(msg: String, pos: SourcePosition = NoSourcePosition)(implicit ctx: Context): Unit =
    reporter.incomplete(new Error(msg, pos))(ctx)

  /** Log msg if settings.log contains the current phase.
   *  See [[config.CompilerCommand#explainAdvanced]] for the exact meaning of
   *  "contains" here.
   */
  def log(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    if (this.settings.log.value.containsPhase(phase))
      echo(s"[log ${ctx.phasesStack.reverse.mkString(" -> ")}] $msg", pos)

  def debuglog(msg: => String): Unit =
    if (ctx.debug) log(msg)

  def informTime(msg: => String, start: Long): Unit = {
    def elapsed = s" in ${currentTimeMillis - start}ms"
    informProgress(msg + elapsed)
  }

  def informProgress(msg: => String) =
    inform("[" + msg + "]")

  def trace[T](msg: => String)(value: T) = {
    log(msg + " " + value)
    value
  }

  def debugwarn(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    if (this.settings.debug.value) warning(msg, pos)

  def debugTraceIndented[T](question: => String, printer: Printers.Printer = Printers.default, show: Boolean = false)(op: => T): T =
    conditionalTraceIndented(this.settings.debugTrace.value, question, printer, show)(op)

  def conditionalTraceIndented[T](cond: Boolean, question: => String, printer: Printers.Printer = Printers.default, show: Boolean = false)(op: => T): T =
    if (cond) traceIndented(question, printer, show)(op)
    else op

  def traceIndented[T](question: => String, printer: Printers.Printer = Printers.default, show: Boolean = false)(op: => T): T = {
    def resStr(res: Any): String = res match {
      case res: printing.Showable if show => res.show
      case _ => String.valueOf(res)
    }
    if (printer eq config.Printers.noPrinter) op
    else {
      // Avoid evaluating question multiple time, since each evaluation
      // may cause some extra logging output.
      lazy val q: String = question
      traceIndented[T](s"==> $q?", (res: Any) => s"<== $q = ${resStr(res)}")(op)
    }
  }

  def traceIndented[T](leading: => String, trailing: Any => String)(op: => T): T =
    if (ctx.mode.is(Mode.Printing)) op
    else {
      var finalized = false
      var logctx = this
      while (logctx.reporter.isInstanceOf[StoreReporter]) logctx = logctx.outer
      def finalize(result: Any, note: String) =
        if (!finalized) {
          base.indent -= 1
          logctx.log(s"${base.indentTab * base.indent}${trailing(result)}$note")
          finalized = true
        }
    try {
      logctx.log(s"${base.indentTab * base.indent}$leading")
      base.indent += 1
      val res = op
      finalize(res, "")
      res
    } catch {
      case ex: Throwable =>
        finalize("<missing>", s" (with exception $ex)")
        throw ex
    }
  }
}

/**
 * This interface provides methods to issue information, warning and
 * error messages.
 */
abstract class Reporter extends interfaces.ReporterResult {

  /** Report a diagnostic */
  def doReport(d: Diagnostic)(implicit ctx: Context): Unit

  /** Whether very long lines can be truncated.  This exists so important
   *  debugging information (like printing the classpath) is not rendered
   *  invisible due to the max message length.
   */
  private var _truncationOK: Boolean = true
  def truncationOK = _truncationOK
  def withoutTruncating[T](body: => T): T = {
    val saved = _truncationOK
    _truncationOK = false
    try body
    finally _truncationOK = saved
  }

  type ErrorHandler = Diagnostic => Context => Unit
  private var incompleteHandler: ErrorHandler = d => c => report(d)(c)
  def withIncompleteHandler[T](handler: ErrorHandler)(op: => T): T = {
    val saved = incompleteHandler
    incompleteHandler = handler
    try op
    finally incompleteHandler = saved
  }

  var errorCount = 0
  var warningCount = 0
  def hasErrors = errorCount > 0
  def hasWarnings = warningCount > 0
  private var errors: List[Error] = Nil
  def allErrors = errors

  /** Have errors been reported by this reporter, or in the
   *  case where this is a StoreReporter, by an outer reporter?
   */
  def errorsReported = hasErrors

  private[this] var reportedFeaturesUseSites = Set[Symbol]()
  def isReportedFeatureUseSite(featureTrait: Symbol): Boolean = reportedFeaturesUseSites.contains(featureTrait)
  def reportNewFeatureUseSite(featureTrait: Symbol): Unit = reportedFeaturesUseSites += featureTrait

  val unreportedWarnings = new mutable.HashMap[String, Int] {
    override def default(key: String) = 0
  }

  def report(d: Diagnostic)(implicit ctx: Context): Unit =
    if (!isHidden(d)) {
      doReport(d)(ctx.addMode(Mode.Printing))
      d match {
        case d: ConditionalWarning if !d.enablingOption.value => unreportedWarnings(d.enablingOption.name) += 1
        case d: Warning => warningCount += 1
        case d: Error =>
          errors = d :: errors
          errorCount += 1
        case d: Info => // nothing to do here
        // match error if d is something else
      }
    }

  def incomplete(d: Diagnostic)(implicit ctx: Context): Unit =
    incompleteHandler(d)(ctx)


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
  private def countString(n: Int, elements: String): String = n match {
    case 0 => "no " + elements + "s"
    case 1 => "one " + elements
    case 2 => "two " + elements + "s"
    case 3 => "three " + elements + "s"
    case 4 => "four " + elements + "s"
    case _ => n + " " + elements + "s"
  }

  /** Should this diagnostic not be reported at all? */
  def isHidden(d: Diagnostic)(implicit ctx: Context): Boolean = ctx.mode.is(Mode.Printing)

  /** Does this reporter contain not yet reported errors or warnings? */
  def hasPending: Boolean = false

  /** Issue all error messages in this reporter to next outer one, or make sure they are written. */
  def flush()(implicit ctx: Context): Unit = {}
}
