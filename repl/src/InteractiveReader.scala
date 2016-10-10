package dotty.tools
package dotc
package repl

import dotc.core.Contexts.Context

/** Reads lines from an input stream */
trait InteractiveReader {
  def readLine(prompt: String): String
  val interactive: Boolean
}

/** The current Scala REPL know how to do this flexibly.
 */
object InteractiveReader {
  /** Create an interactive reader */
  def createDefault(in: Interpreter)(implicit ctx: Context): InteractiveReader = {
    new AmmoniteReader(in)
  }
}
