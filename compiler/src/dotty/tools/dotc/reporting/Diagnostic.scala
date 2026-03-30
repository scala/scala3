package dotty.tools
package dotc
package reporting

import dotty.tools.dotc.config.Settings.Setting
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.interfaces.Diagnostic.{ERROR, INFO, WARNING}
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.chaining.*

import java.util.{Collections, Optional, List => JList}
import core.Decorators.toMessage

object Diagnostic:

  def shouldExplain(dia: Diagnostic)(using Context): Boolean =
    ctx.settings.explain.value && dia.msg.canExplain
    || ctx.settings.explainTypes.value && dia.msg.isInstanceOf[TypeMismatchMsg]
        // keep old explain-types behavior for backwards compatibility and cross-compilation

  // `Diagnostics to be consumed by `Reporter` ---------------------- //
  class Error(
    msg: Message,
    pos: SourcePosition
  ) extends Diagnostic(msg, pos, ERROR):
    def this(str: => String, pos: SourcePosition) = this(str.toMessage, pos)

  def Error(msg: Message, pos: SourcePosition): Error = new Error(msg, pos)
  def Error(str: => String, pos: SourcePosition): Error = new Error(str, pos)

  /** A sticky error is an error that should not be hidden by backtracking and
   *  trying some alternative path. Typically, errors issued after catching
   *  a TypeError exception are sticky.
   */
  class StickyError(
    msg: Message,
    pos: SourcePosition
  ) extends Error(msg, pos)

  /** A Warning with an origin. The semantics of `origin` depend on the warning.
   *  For example, an unused import warning has an origin that specifies the unused selector.
   *  The origin of a deprecation is the deprecated element.
   */
  trait OriginWarning(val origin: String):
    self: Warning =>
  object OriginWarning:
    val NoOrigin = "..."

  /** Lints are likely to be filtered. Provide extra axes for filtering by `-Wconf`.
   */
  class LintWarning(msg: Message, pos: SourcePosition, origin: String = OriginWarning.NoOrigin)
  extends Warning(msg, pos), OriginWarning(origin)

  class Warning(
    msg: Message,
    pos: SourcePosition
  ) extends Diagnostic(msg, pos, WARNING) {
    def toError: Error = Error(msg, pos).tap(e => if isVerbose then e.setVerbose())
    def toInfo: Info = Info(msg, pos).tap(e => if isVerbose then e.setVerbose())
    def isSummarizedConditional(using Context): Boolean = false
  }

  class Info(
    msg: Message,
    pos: SourcePosition
  ) extends Diagnostic(msg, pos, INFO):
    def this(str: => String, pos: SourcePosition) = this(str.toMessage, pos)

  abstract class ConditionalWarning(
    msg: Message,
    pos: SourcePosition
  ) extends Warning(msg, pos) {
    def enablingOption(using Context): Setting[Boolean]
    override def isSummarizedConditional(using Context): Boolean = !enablingOption.value
  }

  class FeatureWarning(
    msg: Message,
    pos: SourcePosition
  ) extends ConditionalWarning(msg, pos) {
    def enablingOption(using Context): Setting[Boolean] = ctx.settings.feature
  }

  class UncheckedWarning(
    msg: Message,
    pos: SourcePosition
  ) extends ConditionalWarning(msg, pos) {
    def enablingOption(using Context): Setting[Boolean] = ctx.settings.unchecked
  }

  class DeprecationWarning(msg: Message, pos: SourcePosition, origin: String)
  extends ConditionalWarning(msg, pos), OriginWarning(origin):
    def enablingOption(using Context): Setting[Boolean] = ctx.settings.deprecation

  class ConfigurationWarning(msg: Message, pos: SourcePosition) extends ConditionalWarning(msg, pos):
    def enablingOption(using Context): Setting[Boolean] = ConfigurationWarning.setting
    override def isSummarizedConditional(using Context): Boolean = false
  object ConfigurationWarning:
    private val setting = Setting.internal("-configuration", value = true)

  class MigrationWarning(
    msg: Message,
    pos: SourcePosition
  ) extends Warning(msg, pos)

class Diagnostic(
  val msg: Message,
  val pos: SourcePosition,
  val level: Int
) extends interfaces.Diagnostic:
  private var verbose: Boolean = false
  def isVerbose: Boolean = verbose
  def setVerbose(): this.type =
    verbose = true
    this

  override def position: Optional[interfaces.SourcePosition] =
    if (pos.exists && pos.source.exists) Optional.of(pos) else Optional.empty()
  override def message: String =
    msg.message.replaceAll("\u001B\\[[;\\d]*m", "")
  override def diagnosticRelatedInformation: JList[interfaces.DiagnosticRelatedInformation] =
    Collections.emptyList()

  override def toString: String = s"$getClass at $pos L${pos.line+1}: $message"
end Diagnostic
