package dotty.tools.pc
package completions

import java.net.URI

import scala.meta.pc.OffsetParams

import dotty.tools.dotc.ast.untpd.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans.*
import dotty.tools.dotc.interactive.Completion
import dotty.tools.pc.utils.InteractiveEnrichments.*

import org.eclipse.lsp4j as l

case object Cursor:
  val value = "CURSOR"

case class CompletionPos(
  queryStart: Int,
  identEnd: Int,
  query: String,
  originalCursorPosition: SourcePosition,
  sourceUri: URI,
  withCURSOR: Boolean
):
  def queryEnd: Int = originalCursorPosition.point
  def stripSuffixEditRange: l.Range = new l.Range(originalCursorPosition.offsetToPos(queryStart), originalCursorPosition.offsetToPos(identEnd))
  def toEditRange: l.Range = originalCursorPosition.withStart(queryStart).withEnd(originalCursorPosition.point).toLsp
  def toSourcePosition: SourcePosition = originalCursorPosition.withSpan(Span(queryStart, queryEnd, queryEnd))

object CompletionPos:

  def infer(
      sourcePos: SourcePosition,
      offsetParams: OffsetParams,
      adjustedPath: List[Tree],
      wasCursorApplied: Boolean
  )(using Context): CompletionPos =
    val identEnd = adjustedPath match
      case (refTree: RefTree) :: _ if refTree.name.toString.contains(Cursor.value) =>
        refTree.span.end - Cursor.value.length
      case (refTree: RefTree) :: _  => refTree.span.end
      case _ => sourcePos.end

    val query = Completion.completionPrefix(adjustedPath, sourcePos)
    val start = sourcePos.end - query.length()

    CompletionPos(start, identEnd, query.nn, sourcePos, offsetParams.uri.nn, wasCursorApplied)

  /**
   * Infer the indentation by counting the number of spaces in the given line.
   *
   * @param lineOffset the offset position of the beginning of the line
   */
  private[completions] def inferIndent(
      lineOffset: Int,
      text: String
  ): (Int, Boolean) =
    var i = 0
    var tabIndented = false
    while lineOffset + i < text.length() && {
        val char = text.charAt(lineOffset + i)
        if char == '\t' then
          tabIndented = true
          true
        else char == ' '
      }
    do i += 1
    (i, tabIndented)
  end inferIndent

end CompletionPos
