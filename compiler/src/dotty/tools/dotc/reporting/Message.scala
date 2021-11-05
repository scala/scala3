package dotty.tools
package dotc
package reporting
import scala.annotation.threadUnsafe
import util.SourcePosition

object Message {
  val nonSensicalStartTag: String = "<nonsensical>"
  val nonSensicalEndTag: String = "</nonsensical>"

  /** This implicit conversion provides a fallback for error messages that have
    * not yet been ported to the new scheme. Comment out this `implicit def` to
    * see where old errors still exist
    */
  implicit def toNoExplanation(str: => String): Message = NoExplanation(str)
}

/** A `Message` contains all semantic information necessary to easily
  * comprehend what caused the message to be logged. Each message can be turned
  * into a `Diagnostic` which contains the log level and can later be
  * consumed by a subclass of `Reporter`. However, the error position is only
  * part of `Diagnostic`, not `Message`.
  *
  * NOTE: you should not persist a message directly, because most messages take
  * an implicit `Context` and these contexts weigh in at about 4mb per instance.
  * Therefore, persisting these will result in a memory leak.
  *
  * Instead use the `persist` method to create an instance that does not keep a
  * reference to these contexts.
  *
  * @param errorId a unique id identifying the message, this will later be
  *                used to reference documentation online
  */
abstract class Message(val errorId: ErrorMessageID) { self =>
  import Message._

  /** The `msg` contains the diagnostic message e.g:
    *
    * > expected: String
    * > found:    Int
    *
    * This message will be placed underneath the position given by the enclosing
    * `Diagnostic`. The message is given in raw form, with possible embedded
    *  <nonsensical> tags.
    */
  protected def msg: String

  /** The kind of the error message, e.g. "Syntax" or "Type Mismatch".
    * This will be printed as "$kind Error", "$kind Warning", etc, on the first
    * line of the message.
    */
  def kind: String

  /** The explanation should provide a detailed description of why the error
    * occurred and use examples from the user's own code to illustrate how to
    * avoid these errors. It might contain embedded <nonsensical> tags.
    */
  protected def explain: String

  /** A message suffix that can be added for certain subclasses */
  protected def msgSuffix: String = ""

  /** Does this message have an explanation?
   *  This is normally the same as `explain.nonEmpty` but can be overridden
   *  if we need a way to return `true` without actually calling the
   *  `explain` method.
   */
  def canExplain: Boolean = explain.nonEmpty

  private var myMsg: String | Null = null
  private var myIsNonSensical: Boolean = false

  private def dropNonSensical(msg: String): String =
    if msg.contains(nonSensicalStartTag) then
      myIsNonSensical = true
      // myMsg might be composed of several d"..." invocations -> nested
      // nonsensical tags possible
      msg
        .replace(nonSensicalStartTag, "")
        .replace(nonSensicalEndTag, "")
    else msg

  /** The message with potential embedded <nonsensical> tags */
  def rawMessage = message

  /** The message to report. <nonsensical> tags are filtered out */
  @threadUnsafe lazy val message: String = dropNonSensical(msg + msgSuffix)

  /** The explanation to report. <nonsensical> tags are filtered out */
  @threadUnsafe lazy val explanation: String = dropNonSensical(explain)

  /** A message is non-sensical if it contains references to <nonsensical>
   *  tags.  Such tags are inserted by the error diagnostic framework if a
   *  message contains references to internally generated error types. Normally
   *  we want to suppress error messages referring to types like this because
   *  they look weird and are normally follow-up errors to something that was
   *  diagnosed before.
   */
  def isNonSensical: Boolean = { message; myIsNonSensical }

  /** The implicit `Context` in messages is a large thing that we don't want
    * persisted. This method gets around that by duplicating the message,
    * forcing its `msg` and `explanation` vals and dropping the implicit context
    * that was captured in the original message.
    */
  def persist: Message = new Message(errorId) {
    val kind       = self.kind
    val msg        = self.msg
    val explain    = self.explain
    override val canExplain = self.canExplain
  }

  def append(suffix: => String): Message = mapMsg(_ ++ suffix)

  def mapMsg(f: String => String): Message = new Message(errorId):
    val kind       = self.kind
    def msg        = f(self.msg)
    def explain    = self.explain
    override def canExplain = self.canExplain

  def appendExplanation(suffix: => String): Message = new Message(errorId):
    val kind       = self.kind
    def msg        = self.msg
    def explain    = self.explain ++ suffix
    override def canExplain = true

  /** Override with `true` for messages that should always be shown even if their
   *  position overlaps another messsage of a different class. On the other hand
   *  multiple messages of the same class with overlapping positions will lead
   *  to only a single message of that class to be issued.
   */
  def showAlways = false

  override def toString = msg
}

/** The fallback `Message` containing no explanation and having no `kind` */
class NoExplanation(msgFn: => String) extends Message(ErrorMessageID.NoExplanationID) {
  def msg: String = msgFn
  def explain: String = ""
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
