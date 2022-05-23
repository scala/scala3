package dotty.tools
package dotc
package transform
package init

import ast.tpd._
import core._
import util.SourcePosition
import Decorators._, printing.SyntaxHighlighting
import Types._, Symbols._, Contexts._

object Errors {
  type Errors = Seq[Error]
  val empty: Errors = Nil

  def show(errs: Errors)(using Context): String =
    errs.map(_.show).mkString(", ")

  sealed trait Error {
    def source: Tree
    def trace: Seq[Tree]
    def show(using Context): String

    def issue(using Context): Unit =
      report.warning(show + stacktrace, source.srcPos)

    def toErrors: Errors = this :: Nil

    def stacktrace(using Context): String = if (trace.isEmpty) "" else " Calling trace:\n" + {
      var last: String = ""
      val sb = new StringBuilder
      trace.foreach { tree =>
        val pos = tree.sourcePos
        val prefix = "-> "
        val line =
          if pos.source.exists then
            val loc = "[ " + pos.source.file.name + ":" + (pos.line + 1) + " ]"
            val code = SyntaxHighlighting.highlight(pos.lineContent.trim.nn)
            i"$code\t$loc"
          else
            tree.show
        val positionMarkerLine =
          if pos.exists && pos.source.exists then
            positionMarker(pos)
          else ""

        if (last != line)  sb.append(prefix + line + "\n" + positionMarkerLine )

        last = line
      }
      sb.toString
    }

    /** Used to underline source positions in the stack trace
     *  pos.source must exist
     */
    private def positionMarker(pos: SourcePosition): String = {
      val trimmed = pos.lineContent.takeWhile(c => c.isWhitespace).length
      val padding = pos.startColumnPadding.substring(trimmed).nn + "   "
      val carets =
        if (pos.startLine == pos.endLine)
          "^" * math.max(1, pos.endColumn - pos.startColumn)
        else "^"

      s"$padding$carets\n"
    }

    /** Flatten UnsafePromotion errors
     */
    def flatten: Errors = this match {
      case unsafe: UnsafePromotion => unsafe.errors.flatMap(_.flatten)
      case _ => this :: Nil
    }

    override def toString() = this.getClass.getName.nn
  }

  /** Access non-initialized field */
  case class AccessNonInit(field: Symbol, trace: Seq[Tree]) extends Error {
    def source: Tree = trace.last
    def show(using Context): String =
      "Access non-initialized " + field.show + "."

    override def issue(using Context): Unit =
      report.warning(show + stacktrace, field.srcPos)
  }

  /** Promote a value under initialization to fully-initialized */
  case class PromoteError(msg: String, source: Tree, trace: Seq[Tree]) extends Error {
    def show(using Context): String = msg
  }

  case class AccessCold(field: Symbol, source: Tree, trace: Seq[Tree]) extends Error {
    def show(using Context): String =
      "Access field " + source.show + " on a value with an unknown initialization status."
  }

  case class CallCold(meth: Symbol, source: Tree, trace: Seq[Tree]) extends Error {
    def show(using Context): String =
      "Call method " + source.show + " on a value with an unknown initialization" + "."
  }

  case class CallUnknown(meth: Symbol, source: Tree, trace: Seq[Tree]) extends Error {
    def show(using Context): String =
      val prefix = if meth.is(Flags.Method) then "Calling the external method " else "Accessing the external field"
      prefix + meth.show + " may cause initialization errors" + "."
  }

  /** Promote a value under initialization to fully-initialized */
  case class UnsafePromotion(msg: String, source: Tree, trace: Seq[Tree], errors: Errors) extends Error {
    assert(errors.nonEmpty)
    override def issue(using Context): Unit =
      report.warning(show, source.srcPos)

    def show(using Context): String = {
      var index = 0
      msg + "\n" + stacktrace + "\n" +
        "Promoting the value to fully initialized failed due to the following problem:\n" +
        errors.head.show + errors.head.stacktrace
    }
  }
}