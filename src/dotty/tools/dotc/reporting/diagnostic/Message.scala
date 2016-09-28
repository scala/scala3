package dotty.tools
package dotc
package reporting
package diagnostic

import util.SourcePosition
import core.Contexts.Context

object Message {
  /** This implicit conversion provides a fallback for error messages that have
    * not yet been ported to the new scheme. Comment out this `implicit def` to
    * see where old errors still exist
    */
  implicit def toNoExplanation(str: String): Message =
    new NoExplanation(str)
}

abstract class Message(val errorId: Int) { self =>
  import messages._

  /** The `msg` contains the diagnostic message e.g:
    *
    * > expected: String
    * > found:    Int
    *
    * This message wil be placed underneath the position given by the enclosing
    * `MessageContainer`
    */
  def msg: String

  /** The kind of the error message is something like "Syntax" or "Type
    * Mismatch"
    */
  def kind: String

  /** The explanation should provide a detailed description of why the error
    * occurred and use examples from the user's own code to illustrate how to
    * avoid these errors.
    */
  def explanation: String

  /** It is possible to map `msg` to add details, this is at the loss of
    * precision since the type of the resulting `Message` won't be original
    * extending class
    *
    * @return a `Message` with the mapped message
    */
  def mapMsg(f: String => String) = new Message(errorId) {
    val msg = f(self.msg)
    val kind = self.kind
    val explanation = self.explanation
  }

  /** Enclose this message in an `Error` container */
  def error(pos: SourcePosition) =
    new Error(self, pos, explanation)

  /** Enclose this message in an `Warning` container */
  def warning(pos: SourcePosition) =
    new Warning(self, pos, explanation)

  /** Enclose this message in an `Info` container */
  def info(pos: SourcePosition) =
    new Info(self, pos, explanation)

  /** Enclose this message in an `FeatureWarning` container */
  def featureWarning(pos: SourcePosition) =
    new FeatureWarning(self, pos, explanation)

  /** Enclose this message in an `UncheckedWarning` container */
  def uncheckedWarning(pos: SourcePosition) =
    new UncheckedWarning(self, pos, explanation)

  /** Enclose this message in an `DeprecationWarning` container */
  def deprecationWarning(pos: SourcePosition) =
    new DeprecationWarning(self, pos, explanation)

  /** Enclose this message in an `MigrationWarning` container */
  def migrationWarning(pos: SourcePosition) =
    new MigrationWarning(self, pos, explanation)
}

/** The fallback `Message` containing no explanation and having no `kind` */
class NoExplanation(val msg: String) extends Message(NoExplanation.ID) {
  val explanation = ""
  val kind = ""
}

/** The extractor for `NoExplanation` can be used to check whether any error
  * lacks an explanation
  */
object NoExplanation {
  final val ID = -1

  def unapply(m: Message): Option[Message] =
    if (m.explanation == "") Some(m)
    else None
}
