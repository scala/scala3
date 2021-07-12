package dotty.tools.dotc

import reporting._
import Diagnostic._
import util.{SourcePosition, NoSourcePosition, SrcPos}
import core._
import Contexts._, Symbols._, Decorators._
import config.SourceVersion
import ast._
import config.Feature.sourceVersion
import java.lang.System.currentTimeMillis


object report:

  /** For sending messages that are printed only if -verbose is set */
  def inform(msg: => String, pos: SrcPos = NoSourcePosition)(using Context): Unit =
    if ctx.settings.verbose.value then echo(msg, pos)

  def echo(msg: => String, pos: SrcPos = NoSourcePosition)(using Context): Unit =
    ctx.reporter.report(new Info(msg, pos.sourcePos))

  private def issueWarning(warning: Warning)(using Context): Unit =
    ctx.reporter.report(warning)

  def deprecationWarning(msg: Message, pos: SrcPos = NoSourcePosition)(using Context): Unit =
    issueWarning(new DeprecationWarning(msg, pos.sourcePos))

  def migrationWarning(msg: Message, pos: SrcPos = NoSourcePosition)(using Context): Unit =
    issueWarning(new MigrationWarning(msg, pos.sourcePos))

  def uncheckedWarning(msg: Message, pos: SrcPos = NoSourcePosition)(using Context): Unit =
    issueWarning(new UncheckedWarning(msg, pos.sourcePos))

  def featureWarning(msg: Message, pos: SrcPos = NoSourcePosition)(using Context): Unit =
    issueWarning(new FeatureWarning(msg, pos.sourcePos))

  def featureWarning(feature: String, featureDescription: => String,
      featureUseSite: Symbol, required: Boolean, pos: SrcPos)(using Context): Unit = {
    val req = if (required) "needs to" else "should"
    val fqname = s"scala.language.$feature"

    val explain =
      if ctx.reporter.isReportedFeatureUseSite(featureUseSite) then ""
      else
        ctx.reporter.reportNewFeatureUseSite(featureUseSite)
        s"""
           |See the Scala docs for value $fqname for a discussion
           |why the feature $req be explicitly enabled.""".stripMargin

    def msg = s"""$featureDescription $req be enabled
                 |by adding the import clause 'import $fqname'
                 |or by setting the compiler option -language:$feature.$explain""".stripMargin
    if (required) error(msg, pos)
    else issueWarning(new FeatureWarning(msg, pos.sourcePos))
  }

  def warning(msg: Message, pos: SrcPos = NoSourcePosition)(using Context): Unit =
    issueWarning(new Warning(msg, addInlineds(pos)))

  def error(msg: Message, pos: SrcPos = NoSourcePosition, sticky: Boolean = false)(using Context): Unit =
    val fullPos = addInlineds(pos)
    ctx.reporter.report(if (sticky) new StickyError(msg, fullPos) else new Error(msg, fullPos))
    if ctx.settings.YdebugError.value then Thread.dumpStack()

  def error(ex: TypeError, pos: SrcPos)(using Context): Unit =
    error(ex.toMessage, pos, sticky = true)
    if ctx.settings.YdebugTypeError.value then ex.printStackTrace()

  def errorOrMigrationWarning(msg: Message, pos: SrcPos = NoSourcePosition,
      from: SourceVersion = SourceVersion.defaultSourceVersion)(using Context): Unit =
    if sourceVersion.isAtLeast(from) then
      if sourceVersion.isMigrating && sourceVersion.ordinal <= from.ordinal then migrationWarning(msg, pos)
      else error(msg, pos)

  def restrictionError(msg: Message, pos: SrcPos = NoSourcePosition)(using Context): Unit =
    error(msg.mapMsg("Implementation restriction: " + _), pos)

  def incompleteInputError(msg: Message, pos: SrcPos = NoSourcePosition)(using Context): Unit =
    ctx.reporter.incomplete(new Error(msg, pos.sourcePos))

  /** Log msg if settings.log contains the current phase.
   *  See [[config.CompilerCommand#explainAdvanced]] for the exact meaning of
   *  "contains" here.
   */
  def log(msg: => String, pos: SrcPos = NoSourcePosition)(using Context): Unit =
    if (ctx.settings.Ylog.value.containsPhase(ctx.phase))
      echo(s"[log ${ctx.phase}] $msg", pos)

  def debuglog(msg: => String)(using Context): Unit =
    if (ctx.debug) log(msg)

  def informTime(msg: => String, start: Long)(using Context): Unit = {
    def elapsed = s" in ${currentTimeMillis - start}ms"
    informProgress(msg + elapsed)
  }

  def informProgress(msg: => String)(using Context): Unit =
    inform("[" + msg + "]")

  def logWith[T](msg: => String)(value: T)(using Context): T = {
    log(msg + " " + value)
    value
  }

  def debugwarn(msg: => String, pos: SrcPos = NoSourcePosition)(using Context): Unit =
    if (ctx.settings.Ydebug.value) warning(msg, pos)

  private def addInlineds(pos: SrcPos)(using Context): SourcePosition =
    def recur(pos: SourcePosition, inlineds: List[Trees.Tree[?]]): SourcePosition = inlineds match
      case inlined :: inlineds1 => pos.withOuter(recur(inlined.sourcePos, inlineds1))
      case Nil => pos
    recur(pos.sourcePos, tpd.enclosingInlineds)

end report