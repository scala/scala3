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

  /** A part of a multi-span diagnostic, associating text with a source position.
   *  @param text       the message text for this part
   *  @param srcPos     the source position where this part applies
   *  @param isPrimary  whether this is the primary message (true) or a secondary note (false)
   */
  case class DiagnosticPart(text: String, srcPos: SourcePosition, isPrimary: Boolean)

  def shouldExplain(dia: Diagnostic)(using Context): Boolean =
    ctx.settings.explain.value && dia.msg.canExplain
    || ctx.settings.explainTypes.value && dia.msg.isInstanceOf[TypeMismatchMsg]
        // keep old explain-types behavior for backwards compatibility and cross-compilation

  // `Diagnostics to be consumed by `Reporter` ---------------------- //
  class Error(
    msg: Message,
    pos: SourcePosition,
    parts: List[DiagnosticPart] = Nil
  ) extends Diagnostic(msg, pos, ERROR, parts):
    def this(str: => String, pos: SourcePosition) = this(str.toMessage, pos)

  /** A sticky error is an error that should not be hidden by backtracking and
   *  trying some alternative path. Typically, errors issued after catching
   *  a TypeError exception are sticky.
   */
  class StickyError(
    msg: Message,
    pos: SourcePosition,
    parts: List[DiagnosticPart] = Nil
  ) extends Error(msg, pos, parts)

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
  class LintWarning(msg: Message, pos: SourcePosition, origin: String = OriginWarning.NoOrigin, parts: List[DiagnosticPart] = Nil)
  extends Warning(msg, pos, parts), OriginWarning(origin)

  class Warning(
    msg: Message,
    pos: SourcePosition,
    parts: List[DiagnosticPart] = Nil
  ) extends Diagnostic(msg, pos, WARNING, parts) {
    def toError: Error = new Error(msg, pos, parts).tap(e => if isVerbose then e.setVerbose())
    def toInfo: Info = new Info(msg, pos, parts).tap(e => if isVerbose then e.setVerbose())
    def isSummarizedConditional(using Context): Boolean = false
  }

  class Info(
    msg: Message,
    pos: SourcePosition,
    parts: List[DiagnosticPart] = Nil
  ) extends Diagnostic(msg, pos, INFO, parts):
    def this(str: => String, pos: SourcePosition) = this(str.toMessage, pos)

  abstract class ConditionalWarning(
    msg: Message,
    pos: SourcePosition,
    parts: List[DiagnosticPart] = Nil
  ) extends Warning(msg, pos, parts) {
    def enablingOption(using Context): Setting[Boolean]
    override def isSummarizedConditional(using Context): Boolean = !enablingOption.value
  }

  class FeatureWarning(
    msg: Message,
    pos: SourcePosition,
    parts: List[DiagnosticPart] = Nil
  ) extends ConditionalWarning(msg, pos, parts) {
    def enablingOption(using Context): Setting[Boolean] = ctx.settings.feature
  }

  class UncheckedWarning(
    msg: Message,
    pos: SourcePosition,
    parts: List[DiagnosticPart] = Nil
  ) extends ConditionalWarning(msg, pos, parts) {
    def enablingOption(using Context): Setting[Boolean] = ctx.settings.unchecked
  }

  class DeprecationWarning(msg: Message, pos: SourcePosition, origin: String, parts: List[DiagnosticPart] = Nil)
  extends ConditionalWarning(msg, pos, parts), OriginWarning(origin):
    def enablingOption(using Context): Setting[Boolean] = ctx.settings.deprecation

  class ConfigurationWarning(msg: Message, pos: SourcePosition, parts: List[DiagnosticPart] = Nil) extends ConditionalWarning(msg, pos, parts):
    def enablingOption(using Context): Setting[Boolean] = ConfigurationWarning.setting
    override def isSummarizedConditional(using Context): Boolean = false
  object ConfigurationWarning:
    private val setting = Setting.internal("-configuration", value = true)

  class MigrationWarning(
    msg: Message,
    pos: SourcePosition,
    parts: List[DiagnosticPart] = Nil
  ) extends Warning(msg, pos, parts)

class Diagnostic(
  val msg: Message,
  val pos: SourcePosition,
  val level: Int,
  val parts: List[Diagnostic.DiagnosticPart] = Nil
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

