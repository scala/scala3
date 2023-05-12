package scala.meta.internal.pc

import java.nio.file.Paths
import java.{util as ju}

import scala.collection.JavaConverters.*

import scala.meta.inputs.Position
import scala.meta.internal.mtags.MtagsEnrichments.*
import scala.meta.internal.pc.SelectionRangeProvider.*
import scala.meta.pc.OffsetParams
import scala.meta.tokens.Token
import scala.meta.tokens.Token.Trivia

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.semanticdb.Scala3
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.SourcePosition
import org.eclipse.lsp4j
import org.eclipse.lsp4j.SelectionRange

/**
 * Provides the functionality necessary for the `textDocument/selectionRange` request.
 *
 * @param compiler Metals Global presentation compiler wrapper.
 * @param params offset params converted from the selectionRange params.
 */
class SelectionRangeProvider(
    driver: InteractiveDriver,
    params: ju.List[OffsetParams],
):

  /**
   * Get the seletion ranges for the provider params
   *
   * @return selection ranges
   */
  def selectionRange(): List[SelectionRange] =
    given ctx: Context = driver.currentCtx

    val selectionRanges = params.asScala.toList.map { param =>

      val uri = param.uri
      val filePath = Paths.get(uri)
      val source = SourceFile.virtual(filePath.toString, param.text)
      driver.run(uri, source)
      val pos = driver.sourcePosition(param)
      val path =
        Interactive.pathTo(driver.openedTrees(uri), pos)(using ctx)

      val bareRanges = path
        .map { tree =>
          val selectionRange = new SelectionRange()
          selectionRange.setRange(tree.sourcePos.toLsp)
          selectionRange
        }

      // if cursor is in comment return range in comment4
      val commentRanges = getCommentRanges(pos, path, param.text()).map { x =>
        new SelectionRange():
          setRange(x)
      }.toList

      (commentRanges ++ bareRanges)
        .reduceRightOption(setParent)
        .getOrElse(new SelectionRange())
    }

    selectionRanges
  end selectionRange

  private def setParent(
      child: SelectionRange,
      parent: SelectionRange,
  ): SelectionRange =
    // If the parent and the child have the same exact range we just skip it.
    // This happens in a lot of various places. For example:
    //
    // val total = for {
    //   a <- >>region>>Some(1)<<region<<
    // } yield a
    //
    // Apply(
    //  Select(Apply(Ident(Some), List(Literal(Constant(1)))), flatMap), <-- This range
    //  List(
    //    Function(
    //      List(ValDef(Modifiers(8192L, , List()), a, <type ?>, <empty>)),
    //      Apply(
    //        Select(Apply(Ident(Some), List(Literal(Constant(2)))), map), <-- Same as this range
    //        ...
    //      )
    //    )
    //  )
    // )
    if child.getRange() == parent.getRange() then parent
    else
      child.setParent(parent)
      child

end SelectionRangeProvider

object SelectionRangeProvider:

  import scala.meta.dialects.Scala3
  import scala.meta.*
  import scala.meta.Token.Comment
  import dotty.tools.dotc.ast.tpd

  def commentRangesFromTokens(
      tokenList: List[Token],
      cursorStart: SourcePosition,
      offsetStart: Int,
  ) =
    val cursorStartShifted = cursorStart.start - offsetStart

    tokenList
      .collect { case x: Comment =>
        (x.start, x.end, x.pos)
      }
      .collect {
        case (commentStart, commentEnd, _)
            if commentStart <= cursorStartShifted && cursorStartShifted <= commentEnd =>
          cursorStart
            .withStart(commentStart + offsetStart)
            .withEnd(commentEnd + offsetStart)
            .toLsp

      }
  end commentRangesFromTokens

  /** get comments under cursor */
  def getCommentRanges(
      cursor: SourcePosition,
      path: List[tpd.Tree],
      srcText: String,
  )(using Context): List[lsp4j.Range] =
    val (treeStart, treeEnd) = path.headOption
      .map(t => (t.sourcePos.start, t.sourcePos.end))
      .getOrElse((0, srcText.size))

    // only parse comments from first range to reduce computation
    val srcSliced = srcText.slice(treeStart, treeEnd)

    val tokens = srcSliced.tokenize.toOption
    if tokens.isEmpty then Nil
    else
      commentRangesFromTokens(
        tokens.toList.flatten,
        cursor,
        treeStart,
      )
  end getCommentRanges
end SelectionRangeProvider
