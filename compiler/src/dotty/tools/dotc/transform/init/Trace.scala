package dotty.tools.dotc
package transform
package init

import core.*
import Contexts.*
import ast.tpd.*
import util.SourcePosition

import Decorators._, printing.SyntaxHighlighting

import scala.collection.mutable

/** Logic related to evaluation trace for showing friendly error messages
 *
 *  A trace is a sequence of program positions which tells the evaluation order
 *  that leads to an error. It is usually more informative than the stack trace
 *  by tracking the exact sub-expression in the trace instead of only methods.
 */
object Trace:
  opaque type Trace = Vector[Tree]

  val empty: Trace = Vector.empty

  extension (trace: Trace)
    def add(node: Tree): Trace = trace :+ node
    def toVector: Vector[Tree] = trace
    def ++(trace2: Trace): Trace = trace ++ trace2

  def show(using trace: Trace, ctx: Context): String = buildStacktrace(trace, "\n")

  def position(using trace: Trace): Tree = trace.last

  def trace(using t: Trace): Trace = t

  inline def withTrace[T](t: Trace)(op: Trace ?=> T): T = op(using t)

  inline def extendTrace[T](node: Tree)(using t: Trace)(op: Trace ?=> T): T = op(using t.add(node))

  def buildStacktrace(trace: Trace, preamble: String)(using Context): String = if trace.isEmpty then "" else preamble + {
    var lastLineNum = -1
    var lines: mutable.ArrayBuffer[String] = new mutable.ArrayBuffer
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

      // always use the more precise trace location
      if lastLineNum == pos.line then
        lines.dropRightInPlace(1)

      lines += (prefix + line + "\n" + positionMarkerLine)

      lastLineNum = pos.line
    }
    val sb = new StringBuilder
    for line <- lines do sb.append(line)
    sb.toString
  }

  /** Used to underline source positions in the stack trace
   *  pos.source must exist
   */
  private def positionMarker(pos: SourcePosition): String =
    val trimmed = pos.lineContent.takeWhile(c => c.isWhitespace).length
    val padding = pos.startColumnPadding.substring(trimmed).nn + "   "
    val carets =
      if (pos.startLine == pos.endLine)
        "^" * math.max(1, pos.endColumn - pos.startColumn)
      else "^"

    s"$padding$carets\n"
