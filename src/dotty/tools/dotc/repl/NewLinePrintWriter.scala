package dotty.tools
package dotc
package repl
import java.io.{Writer, PrintWriter}

class NewLinePrintWriter(out: Writer, autoFlush: Boolean)
extends PrintWriter(out, autoFlush) {
  def this(out: Writer) = this(out, false)
  override def println(): Unit = { print("\n"); flush() }
}

