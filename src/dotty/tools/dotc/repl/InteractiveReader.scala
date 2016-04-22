package dotty.tools
package dotc
package repl

import dotc.core.Contexts.Context

/** Reads lines from an input stream */
trait InteractiveReader {
  def readLine(prompt: String)(implicit ctx: Context): String
  val interactive: Boolean
}

/** The current Scala REPL know how to do this flexibly.
 */
object InteractiveReader {
  /** Create an interactive reader.  Uses JLine if the
   *  library is available, but otherwise uses a
   *  SimpleReader. */
  def createDefault(): InteractiveReader = {
    try {
      new AmmoniteReader()
    } catch { case e =>
      //out.println("jline is not available: " + e) //debug
      e.printStackTrace()
      println("Could not use ammonite, falling back to simple reader")
      new SimpleReader()
    }
  }
}
