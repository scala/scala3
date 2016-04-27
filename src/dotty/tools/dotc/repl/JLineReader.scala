package dotty.tools
package dotc
package repl

import dotc.core.Contexts.Context
import jline.console.ConsoleReader

/** Adaptor for JLine
 */
class JLineReader extends InteractiveReader {
  val reader = new ConsoleReader()

  val interactive = true

  def readLine(prompt: String) = reader.readLine(prompt)
}
