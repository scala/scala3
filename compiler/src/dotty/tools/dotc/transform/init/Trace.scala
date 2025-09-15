package dotty.tools.dotc
package transform
package init

import core.*
import Contexts.*
import ast.tpd.*
import util.SourcePosition
import util.SourceFile

import Decorators.*, printing.SyntaxHighlighting

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

  val EMPTY_PADDING     = "    "
  val CONNECTING_INDENT = "\u2502   "               // "|   "
  val CHILD             = "\u251c\u2500\u2500 "     // "|-- "
  val LAST_CHILD        = "\u2514\u2500\u2500 "     // "\-- "

  extension (trace: Trace)
    def add(node: Tree): Trace = trace :+ node
    def toVector: Vector[Tree] = trace
    def ++(trace2: Trace): Trace = trace ++ trace2

  def show(using trace: Trace, ctx: Context): String = buildStacktrace(trace, "Calling trace:" + System.lineSeparator())

  def position(using trace: Trace): Tree = trace.last

  def trace(using t: Trace): Trace = t

  inline def withTrace[T](t: Trace)(op: Trace ?=> T): T = op(using t)

  inline def extendTrace[T](node: Tree)(using t: Trace)(op: Trace ?=> T): T = op(using t.add(node))

  /**
   * Returns whether the source file exists
   *
   * The method SourceFile#exists always return true thus cannot be used.
   */
  def fileExists(source: SourceFile): Boolean =
    source.content().nonEmpty

  def buildStacktrace(trace: Trace, preamble: String)(using Context): String = if trace.isEmpty then "" else preamble + {
    var lastLineNum = -1
    var lines: mutable.ArrayBuffer[String] = new mutable.ArrayBuffer
    trace.foreach { tree =>
      val isLastTraceItem = tree `eq` trace.last
      val pos = tree.sourcePos
      val hasSource = fileExists(pos.source)
      val line =
        if pos.exists then
          // Show more information for external code without source
          val file = if hasSource then pos.source.file.name else pos.source.file.path
          val loc = file + ":" + (pos.line + 1)
          val code =
            if hasSource then
              SyntaxHighlighting.highlight(pos.lineContent.trim)
            else
              "(no source)"

          i"$code\t[ $loc ]"
        else
          tree match
            case defDef: DefTree =>
              // The definition can be huge, avoid printing the whole definition.
              defDef.symbol.showFullName
            case _ =>
              tree.show.split(System.lineSeparator(), 2).head

      val positionMarkerLine =
        if pos.exists && hasSource then
          (if isLastTraceItem then EMPTY_PADDING else CONNECTING_INDENT)+ positionMarker(pos)
        else
          ""

      // always use the more precise trace location
      if lastLineNum >= 0 && lastLineNum == pos.line then
        lines.dropRightInPlace(1)

      val prefix = if isLastTraceItem then LAST_CHILD else CHILD
      lines += (prefix + line + System.lineSeparator() + positionMarkerLine)

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
    val trimmed = pos.source.lineContent(pos.start).takeWhile(c => c.isWhitespace).length
    val padding = pos.startColumnPadding.substring(trimmed)
    val carets =
      if (pos.startLine == pos.endLine)
        "^" * math.max(1, pos.endColumn - pos.startColumn)
      else "^"

    s"$padding$carets" + System.lineSeparator()
