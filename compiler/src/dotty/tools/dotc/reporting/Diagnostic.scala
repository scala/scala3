package dotty.tools
package dotc
package reporting

import util.SourcePosition
import core.Contexts._
import config.Settings.Setting
import interfaces.Diagnostic.{ERROR, INFO, WARNING}

import java.util.Optional

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
    def toError: Error = new Error(msg, pos)
    def toInfo: Info = new Info(msg, pos)
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
  override def position: Optional[interfaces.SourcePosition] =
    if (pos.exists && pos.source.exists) Optional.of(pos) else Optional.empty()
  override def message: String =
    msg.message.replaceAll("\u001B\\[[;\\d]*m", "")

  override def toString: String = s"$getClass at $pos: $message"
  override def getMessage(): String = message
end Diagnostic
