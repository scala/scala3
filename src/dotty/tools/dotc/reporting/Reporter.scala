package dotty.tools
package dotc
package reporting

import core.Contexts._
import core.Positions._
import core.Decorators.PhaseListDecorator
import collection.mutable
import io.{SourceFile, NoSource}
import java.lang.System.currentTimeMillis

trait Reporting { this: Context =>
  def error(msg: String, pos: Position = NoPosition): Unit = reporter.error(msg, pos)
  def warning(msg: String, pos: Position = NoPosition): Unit = reporter.warning(msg, pos)
  def inform(msg: String, pos: Position = NoPosition): Unit = reporter.info(msg, pos)

  def log(msg: => String): Unit =
    if (this.settings.log.value.containsPhase(phase))
      inform(s"[log ${ctx.phasesStack.reverse.mkString(" -> ")}] $msg")

  def debuglog(msg: => String): Unit =
    if (ctx.debug) log(msg)

  def informTime(msg: => String, start: Long): Unit =
    informProgress(msg + elapsed(start))

  private def elapsed(start: Long) =
    " in " + (currentTimeMillis - start) + "ms"

  def informProgress(msg: => String) =
    if (ctx.settings.verbose.value) inform("[" + msg + "]")

  def trace[T](msg: => String)(value: T) = {
    log(msg + " " + value)
    value
  }

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

object Reporter {
  object Severity extends Enumeration {
    val INFO, WARNING, ERROR = Value
  }
}

/**
 * This interface provides methods to issue information, warning and
 * error messages.
 */
abstract class Reporter {

  import Reporter.Severity.{Value => Severity, _}

  protected def report(msg: String, severity: Severity, pos: Position)(implicit ctx: Context): Unit

  protected def isHidden(severity: Severity, pos: Position)(implicit ctx: Context) = false

  val count = new mutable.HashMap[Severity, Int]() {
    override def default(key: Severity) = 0
  }

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

  type ErrorHandler = (String, Position, Context) => Unit
  private var incompleteHandler: ErrorHandler = error(_, _)(_)
  def withIncompleteHandler[T](handler: ErrorHandler)(op: => T): T = {
    val saved = incompleteHandler
    incompleteHandler = handler
    try op
    finally incompleteHandler = saved
  }

  def hasErrors   = count(ERROR) > 0
  def hasWarnings = count(WARNING) > 0

  /** For sending messages that are printed only if -verbose is set */
  def info(msg: String, pos: Position = NoPosition)(implicit ctx: Context): Unit =
    if (ctx.settings.verbose.value) info0(msg, INFO, pos)

  /** For sending a message which should not be labeled as a warning/error,
   *  but also shouldn't require -verbose to be visible.
   */
  def echo(msg: String, pos: Position = NoPosition)(implicit ctx: Context): Unit =
    info0(msg, INFO, pos)

  def warning(msg: String, pos: Position = NoPosition)(implicit ctx: Context): Unit =
    if (!ctx.settings.nowarn.value)
      withoutTruncating(info0(msg, WARNING, pos))

  def error(msg: String, pos: Position = NoPosition)(implicit ctx: Context): Unit =
    withoutTruncating(info0(msg, ERROR, pos))

  def incompleteInputError(msg: String, pos: Position = NoPosition)(implicit ctx: Context): Unit =
    incompleteHandler(msg, pos, ctx)

  private def info0(msg: String, severity: Severity, pos: Position)(implicit ctx: Context): Unit = {
    if (!isHidden(severity, pos)) {
      count(severity) += 1
      report(msg, severity, pos)
    }
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
    case INFO    => ""
    case ERROR   => "error: "
    case WARNING => "warning: "
  }

  protected def countString(severity: Severity) = {
    assert(severity != INFO)
    countElementsAsString(count(severity), label(severity).dropRight(2))
  }

  def flush(): Unit = {}

  def reset(): Unit = count.clear()
}
