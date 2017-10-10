package dotty.tools
package dotc
package reporting
package diagnostic

import util.SourcePosition
import core.Contexts.Context

import java.util.Optional

object MessageContainer {
  val nonSensicalStartTag = "<nonsensical>"
  val nonSensicalEndTag = "</nonsensical>"

  implicit class MessageContext(val c: Context) extends AnyVal {
    def shouldExplain(cont: MessageContainer): Boolean = {
      implicit val ctx = c
      cont.contained().explanation match {
        case "" => false
        case _ => ctx.settings.explain.value
      }
    }
  }
}

class MessageContainer(
  msgFn: => Message,
  val pos: SourcePosition,
  val level: Int
) extends Exception with interfaces.Diagnostic {
  import MessageContainer._
  private[this] var myMsg: String = null
  private[this] var myIsNonSensical: Boolean = false
  private[this] var myContained: Message = null

  override def position: Optional[interfaces.SourcePosition] =
    if (pos.exists && pos.source.exists) Optional.of(pos) else Optional.empty()

  /** The message to report */
  def message: String = {
    if (myMsg == null) {
      myMsg = contained().msg.replaceAll("\u001B\\[[;\\d]*m", "")
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

  /** This function forces the contained message and returns it */
  def contained(): Message = {
    if (myContained == null)
      myContained = msgFn

    myContained
  }

  /** A message is non-sensical if it contains references to <nonsensical>
   *  tags.  Such tags are inserted by the error diagnostic framework if a
   *  message contains references to internally generated error types. Normally
   *  we want to suppress error messages referring to types like this because
   *  they look weird and are normally follow-up errors to something that was
   *  diagnosed before.
   */
  def isNonSensical = { message; myIsNonSensical }

  override def toString = s"$getClass at $pos: ${message}"
  override def getMessage() = message
}
