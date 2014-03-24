package dotty.tools.dotc

import core.Contexts._
import util.SourcePosition
import reporting.Severity._

package object reporting {

  class Diagnostic(msgFn: => String, val pos: SourcePosition, val severity: Severity, base: ContextBase) extends Exception {
    private var myMsg: String = null
    private var myIsSuppressed: Boolean = false
    def msg: String = {
      if (myMsg == null)
        try myMsg = msgFn
        catch {
          case ex: SuppressedMessage =>
            myIsSuppressed = true
            val saved = base.suppressNonSensicalErrors
            base.suppressNonSensicalErrors = false
            try myMsg = msgFn
            finally base.suppressNonSensicalErrors = saved
        }
      myMsg
    }
    def isSuppressed = { msg; myIsSuppressed }
    override def toString = s"$severity at $pos: $msg"
    override def getMessage() = msg

    def promotedSeverity(implicit ctx: Context): Severity =
      if (isConditionalWarning(severity) && enablingOption(severity).value) WARNING
      else severity
  }

  def Diagnostic(msgFn: => String, pos: SourcePosition, severity: Severity)(implicit ctx: Context) =
    new Diagnostic(msgFn, pos, severity, ctx.base)

  def isConditionalWarning(s: Severity) =
    DeprecationWARNING.level <= s.level && s.level <= FeatureWARNING.level

  val conditionalWarnings = List(DeprecationWARNING, UncheckedWARNING, FeatureWARNING)

  private[reporting] def enablingOption(warning: Severity)(implicit ctx: Context) = warning match {
    case DeprecationWARNING => ctx.settings.deprecation
    case UncheckedWARNING   => ctx.settings.unchecked
    case FeatureWARNING     => ctx.settings.feature
  }

  class SuppressedMessage extends Exception
}
