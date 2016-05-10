package dotty.tools
package dotc
package repl

import java.io.{BufferedReader, PrintWriter}
import dotc.core.Contexts.Context


/** Reads using standard JDK API */
class SimpleReader(
  in: BufferedReader,
  out: PrintWriter,
  val interactive: Boolean)
extends InteractiveReader {
  def this() = this(Console.in, new PrintWriter(Console.out), true)

  def readLine(prompt: String) = {
    if (interactive) {
      out.print(prompt)
      out.flush()
    }
    in.readLine()
  }
}
