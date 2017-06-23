package dotty.tools
package dotc
package repl
import java.io.Writer

/** A Writer that writes onto the Scala Console.
 *
 *  @author  Lex Spoon
 *  @version 1.0
 */
class ConsoleWriter extends Writer {
  def close() = flush()

  def flush() = Console.flush()

  def write(cbuf: Array[Char], off: Int, len: Int): Unit =
    if (len > 0)
      write(new String(cbuf, off, len))

  override def write(str: String): Unit = Console.print(str)
}
