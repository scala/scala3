package dotty.tools
package dotc
package reporting

import dotty.tools.dotc.config.Settings.Setting
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.interfaces.Diagnostic.{ERROR, INFO, WARNING}
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.SourcePosition.inlinePosStack
import dotty.tools.dotc.util.chaining.*

import java.util.{Collections, Optional, List => JList}
import scala.jdk.CollectionConverters.*
import core.Decorators.toMessage

object Diagnostic:

  /** Message attached to each inline call-site exposed as related information (scalameta/metals#3214).
   *  The clickable location is carried by `position()`; the text only has to explain why the
   *  location is relevant, so it is phrased independently of direction.
   */
  val inlinedCodeMessage: String = "Inlined code originated here."

  /** A concrete related-information entry carrying both the (concrete) `pos` and its `message`.
   *  It is the single source from which both the public `interfaces.DiagnosticRelatedInformation`
   *  view and the sbt-bridge `xsbti` view are derived, so the two cannot diverge. The concrete
   *  `pos` lets the bridge reuse its `positionOf` helper without downcasting `interfaces.SourcePosition`.
   */
  case class RelatedInformation(pos: SourcePosition, message: String)
  extends interfaces.DiagnosticRelatedInformation:
    override def position: interfaces.SourcePosition = pos


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

  class OptimizerWarning(
    msg: Message,
    site: String, // TODO: show this
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
  /** Structured related information for this diagnostic (scalameta/metals#3214). Currently the
   *  chain of inline call-sites surrounding the reported position: the filtering mirrors
   *  `MessageRendering.messageAndPos`, so the list matches the rendered "Inline stack trace" and
   *  does not duplicate the primary underline (`pos.nonInlined`). This is the single concrete source
   *  from which both `diagnosticRelatedInformation` and the sbt-bridge `xsbti` view are derived.
   */
  def relatedInformation: List[Diagnostic.RelatedInformation] =
    val outermost = pos.outermost
    pos.inlinePosStack.filterNot(outermost.contains(_)).filter(_.exists)
      .map(p => Diagnostic.RelatedInformation(p, Diagnostic.inlinedCodeMessage))

  /** The structured related information surfaced through the public compiler interface, so tooling
   *  can offer one clickable entry per related location (scalameta/metals#3214).
   */
  override def diagnosticRelatedInformation: JList[interfaces.DiagnosticRelatedInformation] =
    val infos: List[interfaces.DiagnosticRelatedInformation] = relatedInformation
    infos.asJava

  override def toString: String = s"$getClass at $pos L${pos.line+1}: $message"
end Diagnostic
