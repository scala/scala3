package dotty.tools
package dotc
package reporting

import util.SourcePosition

object Diagnostic {

  // Error levels
  val ERROR = 2
  val WARNING = 1
  val INFO = 0

  val nonSensicalStartTag = "<nonsensical>"
  val nonSensicalEndTag = "</nonsensical>"
}

class Diagnostic(msgFn: => String, val pos: SourcePosition, val level: Int) extends Exception {
  import Diagnostic._
  private var myMsg: String = null
  private var myIsNonSensical: Boolean = false

  /** The message to report */
  def message: String = {
    if (myMsg == null) {
      myMsg = msgFn
      if (myMsg.contains(nonSensicalStartTag)) {
        myIsNonSensical = true
        // myMsg might be composed of several d"..." invocations -> nested nonsensical tags possible
        myMsg = myMsg.replaceAllLiterally(nonSensicalStartTag, "").replaceAllLiterally(nonSensicalEndTag, "")
      }
    }
    myMsg
  }

  /** A message is non-sensical if it contains references to <nonsensical> tags.
   *  Such tags are inserted by the error diagnostic framework if a message
   *  contains references to internally generated error types. Normally we
   *  want to suppress error messages referring to types like this because
   *  they look weird and are normally follow-up errors to something that
   *  was diagnosed before.
   */
  def isNonSensical = { message; myIsNonSensical }

  override def toString = s"$getClass at $pos: $message"
  override def getMessage() = message
}
