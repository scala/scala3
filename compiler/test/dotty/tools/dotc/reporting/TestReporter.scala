package dotty.tools
package dotc
package reporting

import scala.collection.mutable
import util.SourcePosition
import core.Contexts._
import Reporter._
import java.io.PrintWriter
import diagnostic.{ Message, MessageContainer, NoExplanation }
import diagnostic.messages._

class TestReporter(writer: PrintWriter) extends Reporter
with UniqueMessagePositions with HideNonSensicalMessages {

  import MessageContainer._

  /** maximal number of error messages to be printed */
  protected def ErrorLimit = 100

  def printPos(pos: SourcePosition): Unit =
    if (pos.exists) {
      if (pos.outer.exists) {
        writer.println(s"\ninlined at ${pos.outer}:\n")
        printPos(pos.outer)
      }
    }

  /** Prints the message with the given position indication. */
  def printMessageAndPos(msg: String, pos: SourcePosition)(implicit ctx: Context): Unit = {
    val posStr = s"${pos.line + 1}: "
    writer.println(posStr + msg)
    printPos(pos)
  }

  override def doReport(m: MessageContainer)(implicit ctx: Context): Unit = {
    // Here we add extra information that we should know about the error message
    val extra = m.contained match {
      case pm: PatternMatchExhaustivity => s": ${pm.uncovered}"
      case _ => ""
    }

    m match {
      case m: Error =>
        printMessageAndPos(m.contained.kind + extra, m.pos)
      case w: Warning =>
        printMessageAndPos(w.contained.kind + extra, w.pos)
      case _ =>
    }
  }
}
