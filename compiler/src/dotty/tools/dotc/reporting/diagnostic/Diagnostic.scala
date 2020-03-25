package dotty.tools
package dotc
package reporting
package diagnostic

import util.SourcePosition
import core.Contexts.Context
import config.Settings.Setting
import interfaces.Diagnostic.{ERROR, INFO, WARNING}

import java.util.Optional

object Diagnostic:
  val nonSensicalStartTag: String = "<nonsensical>"
  val nonSensicalEndTag: String = "</nonsensical>"

  implicit class MessageContext(val c: Context) extends AnyVal {
    def shouldExplain(dia: Diagnostic): Boolean = {
      implicit val ctx = c
      dia.contained.explanation match {
        case "" => false
        case _ => ctx.settings.explain.value
      }
    }
  }

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
  }

  class Info(
    msg: Message,
    pos: SourcePosition
  ) extends Diagnostic(msg, pos, INFO)

  abstract class ConditionalWarning(
    msg: Message,
    pos: SourcePosition
  ) extends Warning(msg, pos) {
    def enablingOption(implicit ctx: Context): Setting[Boolean]
  }

  class FeatureWarning(
    msg: Message,
    pos: SourcePosition
  ) extends ConditionalWarning(msg, pos) {
    def enablingOption(implicit ctx: Context): Setting[Boolean] = ctx.settings.feature
  }

  class UncheckedWarning(
    msg: Message,
    pos: SourcePosition
  ) extends ConditionalWarning(msg, pos) {
    def enablingOption(implicit ctx: Context): Setting[Boolean] = ctx.settings.unchecked
  }

  class DeprecationWarning(
    msg: Message,
    pos: SourcePosition
  ) extends ConditionalWarning(msg, pos) {
    def enablingOption(implicit ctx: Context): Setting[Boolean] = ctx.settings.deprecation
  }

  class MigrationWarning(
    msg: Message,
    pos: SourcePosition
  ) extends ConditionalWarning(msg, pos) {
    def enablingOption(implicit ctx: Context): Setting[Boolean] = ctx.settings.migration
  }

class Diagnostic(
  val contained: Message,
  val pos: SourcePosition,
  val level: Int
) extends Exception with interfaces.Diagnostic {
  import Diagnostic._
  private var myMsg: String = null
  private var myIsNonSensical: Boolean = false
  private var myContained: Message = null

  override def position: Optional[interfaces.SourcePosition] =
    if (pos.exists && pos.source.exists) Optional.of(pos) else Optional.empty()

  /** The message to report */
  def message: String = {
    if (myMsg == null) {
      myMsg = contained.msg.replaceAll("\u001B\\[[;\\d]*m", "")
      if (myMsg.contains(nonSensicalStartTag)) {
        myIsNonSensical = true
        // myMsg might be composed of several d"..." invocations -> nested
        // nonsensical tags possible
        myMsg =
          myMsg
          .replaceAllLiterally(nonSensicalStartTag, "")
          .replaceAllLiterally(nonSensicalEndTag, "")
      }
    }
    myMsg
  }

  /** A message is non-sensical if it contains references to <nonsensical>
   *  tags.  Such tags are inserted by the error diagnostic framework if a
   *  message contains references to internally generated error types. Normally
   *  we want to suppress error messages referring to types like this because
   *  they look weird and are normally follow-up errors to something that was
   *  diagnosed before.
   */
  def isNonSensical: Boolean = { message; myIsNonSensical }

  override def toString: String = s"$getClass at $pos: ${message}"
  override def getMessage(): String = message
}
