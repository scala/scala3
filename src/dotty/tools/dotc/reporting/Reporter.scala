package dotty.tools
package dotc
package reporting

import core.Contexts._
import util.{SourcePosition, NoSourcePosition}
import util.{SourceFile, NoSource}
import core.Decorators.PhaseListDecorator
import collection.mutable
import config.Settings.Setting
import java.lang.System.currentTimeMillis

object Reporter {

  class Diagnostic(msgFn: => String, val pos: SourcePosition, val severity: Severity) extends Exception {
    lazy val msg: String = msgFn
    override def toString = s"$severity at $pos: $msg"
    override def getMessage() = msg
  }

  class Severity(val level: Int) extends AnyVal {
    override def toString = this match {
      case VerboseINFO => "VerboseINFO"
      case INFO => "INFO"
      case DeprecationWARNING => "DeprecationWARNING"
      case UncheckedWARNING => "UncheckedWARNING"
      case FeatureWARNING => "FeatureWARNING"
      case WARNING => "WARNING"
      case ERROR => "ERROR"
    }
  }

  final val VerboseINFO = new Severity(0)
  final val INFO = new Severity(1)
  final val DeprecationWARNING = new Severity(2)
  final val UncheckedWARNING = new Severity(3)
  final val FeatureWARNING = new Severity(4)
  final val WARNING = new Severity(5)
  final val ERROR = new Severity(6)

  def isConditionalWarning(s: Severity) =
    DeprecationWARNING.level <= s.level && s.level <= FeatureWARNING.level

  val conditionalWarnings = List(DeprecationWARNING, UncheckedWARNING, FeatureWARNING)
}

import Reporter._

trait Reporting { this: Context =>

  /** For sending messages that are printed only if -verbose is set */
  def inform(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(new Diagnostic(msg, pos, VerboseINFO))

  def echo(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(new Diagnostic(msg, pos, INFO))

  def deprecationWarning(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(new Diagnostic(msg, pos, DeprecationWARNING))

  def uncheckedWarning(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(new Diagnostic(msg, pos, UncheckedWARNING))

  def featureWarning(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(new Diagnostic(msg, pos, FeatureWARNING))

  def warning(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(new Diagnostic(msg, pos, WARNING))

  def error(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(new Diagnostic(msg, pos, ERROR))

  def incompleteInputError(msg: String, pos: SourcePosition = NoSourcePosition)(implicit ctx: Context): Unit =
    reporter.incomplete(new Diagnostic(msg, pos, ERROR))(ctx)

  def log(msg: => String): Unit =
    if (this.settings.log.value.containsPhase(phase))
      echo(s"[log ${ctx.phasesStack.reverse.mkString(" -> ")}] $msg")

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

  def debugTraceIndented[T](question: => String)(op: => T): T =
    if (this.settings.debugTrace.value) traceIndented(question)(op)
    else op

  def traceIndented[T](question: => String)(op: => T): T =
    traceIndented[T](s"==> $question?", (res: Any) => s"<== $question = $res")(op)

  def traceIndented[T](leading: => String, trailing: Any => String)(op: => T): T = {
    var finalized = false
    def finalize(result: Any, note: String) =
      if (!finalized) {
        base.indent -= 1
        log(s"${base.indentTab * base.indent}${trailing(result)}$note")
        finalized = true
      }
    try {
      log(s"${base.indentTab * base.indent}$leading")
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
abstract class Reporter {

  /** Report a diagnostic */
  protected def doReport(d: Diagnostic)(implicit ctx: Context): Unit

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

  protected def isHidden(s: Severity, pos: SourcePosition)(implicit ctx: Context) = s match {
    case VerboseINFO => !ctx.settings.verbose.value
    case DeprecationWARNING | UncheckedWARNING | FeatureWARNING => true
    case _ => false
  }

  private def enablingOption(warning: Severity)(implicit ctx: Context) = warning match {
    case DeprecationWARNING => ctx.settings.deprecation
    case UncheckedWARNING   => ctx.settings.unchecked
    case FeatureWARNING     => ctx.settings.feature
  }

  private def promoteSeverity(s: Severity)(implicit ctx: Context): Severity =
    if (isConditionalWarning(s) && enablingOption(s).value) WARNING else s

  val count = new Array[Int](ERROR.level + 1)

  def report(d: Diagnostic)(implicit ctx: Context): Unit = {
    val severity = promoteSeverity(d.severity)
    if (!isHidden(severity, d.pos)) doReport(d)
    count(severity.level) += 1
  }

  def incomplete(d: Diagnostic)(implicit ctx: Context): Unit =
    incompleteHandler(d)(ctx)

  def hasErrors   = count(ERROR.level) > 0
  def hasWarnings = count(WARNING.level) > 0

  /** Returns a string meaning "n elements". */
  private def countElementsAsString(n: Int, elements: String): String =
    n match {
      case 0 => "no "    + elements + "s"
      case 1 => "one "   + elements
      case 2 => "two "   + elements + "s"
      case 3 => "three " + elements + "s"
      case 4 => "four "  + elements + "s"
      case _ => n + " " + elements + "s"
    }

  protected def label(severity: Severity): String = severity match {
    case ERROR   => "error: "
    case WARNING => "warning: "
    case _    => ""
  }

  protected def countString(severity: Severity) = {
    assert(severity.level >= WARNING.level)
    countElementsAsString(count(severity.level), label(severity).dropRight(2))
  }

  def printSummary(implicit ctx: Context) {
    if (count(WARNING.level) > 0) ctx.echo(countString(WARNING) + " found")
    if (  count(ERROR.level) > 0) ctx.echo(countString(ERROR  ) + " found")
    for (cwarning <- conditionalWarnings) {
      val unreported = count(cwarning.level)
      if (unreported > 0) {
        val what = enablingOption(cwarning).name.tail
        ctx.warning(s"there were $unreported $what warning(s); re-run with -$what for details")
      }
    }
  }

  def flush()(implicit ctx: Context): Unit = {}
}
