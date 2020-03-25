package dotty.tools
package dotc
package reporting
package diagnostic

import util.SourcePosition

import messages._

object Message {
  /** This implicit conversion provides a fallback for error messages that have
    * not yet been ported to the new scheme. Comment out this `implicit def` to
    * see where old errors still exist
    */
  implicit def toNoExplanation(str: => String): Message =
    new NoExplanation(str)
}

/** A `Message` contains all semantic information necessary to easily
  * comprehend what caused the message to be logged. Each message can be turned
  * into a `MessageContainer` which contains the log level and can later be
  * consumed by a subclass of `Reporter`. However, the error position is only
  * part of `MessageContainer`, not `Message`.
  *
  * NOTE: you should not be persisting  Most messages take an implicit
  * `Context` and these contexts weigh in at about 4mb per instance, as such
  * persisting these will result in a memory leak.
  *
  * Instead use the `persist` method to create an instance that does not keep a
  * reference to these contexts.
  *
  * @param errorId a unique id identifying the message, this will later be
  *                used to reference documentation online
  */
abstract class Message(val errorId: ErrorMessageID) { self =>

  /** The `msg` contains the diagnostic message e.g:
    *
    * > expected: String
    * > found:    Int
    *
    * This message will be placed underneath the position given by the enclosing
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

  /** The implicit `Context` in messages is a large thing that we don't want
    * persisted. This method gets around that by duplicating the message,
    * forcing its `msg` and `explanation` vals and dropping the implicit context
    * that was captured in the original message.
    */
  def persist: Message = new Message(errorId) {
    val kind        = self.kind
    val msg         = self.msg
    val explanation = self.explanation
  }

  def append(suffix: => String): Message = mapMsg(_ ++ suffix)

  def mapMsg(f: String => String): Message = new Message(errorId):
    val kind             = self.kind
    lazy val msg         = f(self.msg)
    lazy val explanation = self.explanation

  def appendExplanation(suffix: => String): Message = new Message(errorId):
    val kind             = self.kind
    lazy val msg         = self.msg
    lazy val explanation = self.explanation ++ suffix

  override def toString = msg
}

/** The fallback `Message` containing no explanation and having no `kind` */
class NoExplanation(msgFn: => String) extends Message(ErrorMessageID.NoExplanationID) {
  lazy val msg: String = msgFn
  val explanation: String = ""
  val kind: String = ""

  override def toString(): String = msg
}

/** The extractor for `NoExplanation` can be used to check whether any error
  * lacks an explanation
  */
object NoExplanation {
  def unapply(m: Message): Option[Message] =
    if (m.explanation == "") Some(m)
    else None
}
