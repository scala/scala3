package dotty.tools.pc
package completions

import java.net.URI

import scala.meta.pc.OffsetParams

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.ast.untpd.ImportSelector
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.util.Chars
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans
import dotty.tools.pc.utils.MtagsEnrichments.*

import org.eclipse.lsp4j as l

enum CompletionKind:
  case Empty, Scope, Members

case class CompletionPos(
    kind: CompletionKind,
    start: Int,
    end: Int,
    query: String,
    cursorPos: SourcePosition,
    sourceUri: URI
):

  def sourcePos: SourcePosition = cursorPos.withSpan(Spans.Span(start, end))
  def stripSuffixEditRange: l.Range = new l.Range(cursorPos.offsetToPos(start), cursorPos.offsetToPos(end))
  def toEditRange: l.Range = cursorPos.withStart(start).withEnd(cursorPos.point).toLsp

end CompletionPos

object CompletionPos:

  def infer(
      cursorPos: SourcePosition,
      offsetParams: OffsetParams,
      treePath: List[Tree]
  )(using Context): CompletionPos =
    infer(cursorPos, offsetParams.uri().nn, offsetParams.text().nn, treePath)

  def infer(
      cursorPos: SourcePosition,
      uri: URI,
      text: String,
      treePath: List[Tree]
  )(using Context): CompletionPos =
    val start = inferIdentStart(cursorPos, text, treePath)
    val end = inferIdentEnd(cursorPos, text)
    val query = text.substring(start, end)
    val prevIsDot =
      if start - 1 >= 0 then text.charAt(start - 1) == '.' else false
    val kind =
      if query.nn.isEmpty() && !prevIsDot then CompletionKind.Empty
      else if prevIsDot then CompletionKind.Members
      else CompletionKind.Scope

    CompletionPos(kind, start, end, query.nn, cursorPos, uri)
  end infer

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

  /**
   * Returns the start offset of the identifier starting as the given offset position.
   */
  private def inferIdentStart(
      pos: SourcePosition,
      text: String,
      path: List[Tree]
  )(using Context): Int =
    def fallback: Int =
      var i = pos.point - 1
      while i >= 0 && Chars.isIdentifierPart(text.charAt(i)) do i -= 1
      i + 1
    def loop(enclosing: List[Tree]): Int =
      enclosing match
        case Nil => fallback
        case head :: tl =>
          if !head.sourcePos.contains(pos) then loop(tl)
          else
            head match
              case i: Ident => i.sourcePos.point
              case s: Select =>
                if s.name.toTermName == nme.ERROR || s.span.exists && pos.span.point < s.span.point
                then fallback
                else s.span.point
              case Import(_, sel) =>
                sel
                  .collectFirst {
                    case ImportSelector(imported, renamed, _)
                        if imported.sourcePos.contains(pos) =>
                      imported.sourcePos.point
                  }
                  .getOrElse(fallback)
              case _ => fallback
    loop(path)
  end inferIdentStart

  /**
   * Returns the end offset of the identifier starting as the given offset position.
   */
  private def inferIdentEnd(pos: SourcePosition, text: String): Int =
    var i = pos.point
    while i < text.length() && Chars.isIdentifierPart(text.charAt(i)) do i += 1
    i

end CompletionPos
