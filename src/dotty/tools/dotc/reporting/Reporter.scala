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
import Severity._

trait Reporting { this: Context =>

  /** For sending messages that are printed only if -verbose is set */
  def inform(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(Diagnostic(msg, pos, VerboseINFO))

  def echo(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(Diagnostic(msg, pos, INFO))

  def deprecationWarning(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(Diagnostic(msg, pos, DeprecationWARNING))

  def uncheckedWarning(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(Diagnostic(msg, pos, UncheckedWARNING))

  def featureWarning(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(Diagnostic(msg, pos, FeatureWARNING))

  def warning(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(Diagnostic(msg, pos, WARNING))

  def error(msg: => String, pos: SourcePosition = NoSourcePosition): Unit = {
    // println("*** ERROR: " + msg) // !!! DEBUG
    reporter.report(Diagnostic(msg, pos, ERROR))
  }

  def incompleteInputError(msg: String, pos: SourcePosition = NoSourcePosition)(implicit ctx: Context): Unit =
    reporter.incomplete(Diagnostic(msg, pos, ERROR))(ctx)

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
    else traceIndented[T](s"==> $question?", (res: Any) => s"<== $question = ${resStr(res)}")(op)
  }

  def traceIndented[T](leading: => String, trailing: Any => String)(op: => T): T = {
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

  def errorsReported: Boolean = outersIterator exists (_.reporter.hasErrors)
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

  protected def isHidden(d: Diagnostic)(implicit ctx: Context) = d.promotedSeverity match {
    case VerboseINFO => !ctx.settings.verbose.value
    case DeprecationWARNING | UncheckedWARNING | FeatureWARNING => true
    case _ => false
  }

  val count = new Array[Int](ERROR.level + 1)

  def report(d: Diagnostic)(implicit ctx: Context): Unit =
    if (!isHidden(d)) {
      doReport(d)
      count(d.promotedSeverity.level) += 1
    }

  def incomplete(d: Diagnostic)(implicit ctx: Context): Unit =
    incompleteHandler(d)(ctx)

  def hasErrors   = count(ERROR.level) > 0
  def hasWarnings = count(WARNING.level) > 0

  def errorCounts: Any = count.clone

  def wasSilent[T](counts: Any): Boolean = {
    val prevCount = counts.asInstanceOf[Array[Int]]
    var i = 0
    while (i < count.length) {
      if (prevCount(i) != count(i)) return false
      i += 1
    }
    true
  }

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

  def printSummary(implicit ctx: Context): Unit = {
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
