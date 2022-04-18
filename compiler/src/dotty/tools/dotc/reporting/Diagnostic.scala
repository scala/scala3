package dotty.tools
package dotc
package reporting

import scala.language.unsafeNulls

import dotty.tools.dotc.config.Settings.Setting
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.interfaces.Diagnostic.{ERROR, INFO, WARNING}
import dotty.tools.dotc.util.SourcePosition

import java.util.Optional
import scala.util.chaining._

object Diagnostic:

  def shouldExplain(dia: Diagnostic)(using Context): Boolean =
    ctx.settings.explain.value && dia.msg.canExplain
    || ctx.settings.explainTypes.value && dia.msg.isInstanceOf[TypeMismatchMsg]
        // keep old explain-types behavior for backwards compatibility and cross-compilation

  // `Diagnostics to be consumed by `Reporter` ---------------------- //
  class Error(
    msg: Message,
    pos: SourcePosition
  ) extends Diagnostic(msg, pos, ERROR)

  /** A sticky error is an error that should not be hidden by backtracking and
   *  trying some alternative path. Typically, errors issued after catching
   *  a TypeError exception are sticky.
   */
  class StickyError(
    msg: Message,
    pos: SourcePosition
  ) extends Error(msg, pos)

  class Warning(
    msg: Message,
    pos: SourcePosition
  ) extends Diagnostic(msg, pos, WARNING) {
    def toError: Error = new Error(msg, pos).tap(e => if isVerbose then e.setVerbose())
    def toInfo: Info = new Info(msg, pos).tap(e => if isVerbose then e.setVerbose())
    def isSummarizedConditional(using Context): Boolean = false
  }

  class Info(
    msg: Message,
    pos: SourcePosition
  ) extends Diagnostic(msg, pos, INFO)

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

  class DeprecationWarning(
    msg: Message,
    pos: SourcePosition
  ) extends ConditionalWarning(msg, pos) {
    def enablingOption(using Context): Setting[Boolean] = ctx.settings.deprecation
  }

  class MigrationWarning(
    msg: Message,
    pos: SourcePosition
  ) extends Warning(msg, pos)

class Diagnostic(
  val msg: Message,
  val pos: SourcePosition,
  val level: Int
) extends Exception with interfaces.Diagnostic:
  private var verbose: Boolean = false
  def isVerbose: Boolean = verbose
  def setVerbose(): this.type =
    verbose = true
    this

  override def position: Optional[interfaces.SourcePosition] =
    if (pos.exists && pos.source.exists) Optional.of(pos) else Optional.empty()
  override def message: String =
    msg.message.replaceAll("\u001B\\[[;\\d]*m", "")

  override def toString: String = s"$getClass at $pos: $message"
  override def getMessage(): String = message
end Diagnostic
