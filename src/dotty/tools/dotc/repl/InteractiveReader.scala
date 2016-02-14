package dotty.tools
package dotc
package repl

/** Reads lines from an input stream */
trait InteractiveReader {
  def readLine(prompt: String): String
  val interactive: Boolean
}

/** TODO Enable jline support.
 *  The current Scala REPL know how to do this flexibly.
 */
object InteractiveReader {
  /** Create an interactive reader.  Uses JLine if the
   *  library is available, but otherwise uses a
   *  SimpleReader. */
  def createDefault(): InteractiveReader = new SimpleReader()
  /*
  {
    try {
      new JLineReader
    } catch {
      case e =>
        //out.println("jline is not available: " + e) //debug
	      new SimpleReader()
    }
  }
*/

}
