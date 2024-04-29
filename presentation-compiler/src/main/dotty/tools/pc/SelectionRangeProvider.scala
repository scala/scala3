package dotty.tools.pc

import java.nio.file.Paths
import java.util as ju

import scala.jdk.CollectionConverters._
import scala.meta.pc.OffsetParams

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.pc.utils.InteractiveEnrichments.*

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
    params: ju.List[OffsetParams]
):

  /**
   * Get the seletion ranges for the provider params
   *
   * @return selection ranges
   */
  def selectionRange(): List[SelectionRange] =
    given ctx: Context = driver.currentCtx

    params.asScala.toList.map { param =>

      val uri = param.uri().nn
      val text = param.text().nn
      val filePath = Paths.get(uri)
      val source = SourceFile.virtual(filePath.toString, text)
      driver.run(uri, source)
      val pos = driver.sourcePosition(param)
      val path =
        Interactive.pathTo(driver.openedTrees(uri), pos)(using ctx)

      val bareRanges = path
        .flatMap(selectionRangesFromTree(pos))

      val comments =
        driver.compilationUnits.get(uri).map(_.comments).toList.flatten

      val commentRanges = comments
        .find(_.span.contains(pos.span))
        .map { comment =>
          val startLine = source.offsetToLine(comment.span.start)
          val endLine = source.offsetToLine(comment.span.end)
          val startChar = source.column(comment.span.start)
          val endChar = source.column(comment.span.end)

          new SelectionRange():
            setRange(
              new lsp4j.Range(
                lsp4j.Position(startLine, startChar),
                lsp4j.Position(endLine, endChar)
              )
            )
        }
        .toList

      (commentRanges ++ bareRanges)
        .reduceRightOption(setParent)
        .getOrElse(new SelectionRange())
    }
  end selectionRange

  /** Given a tree, create a seq of [[SelectionRange]]s corresponding to that tree. */
  private def selectionRangesFromTree(pos: SourcePosition)(tree: tpd.Tree)(using Context) =
    def toSelectionRange(srcPos: SourcePosition) =
      val selectionRange = new SelectionRange()
      selectionRange.setRange(srcPos.toLsp)
      selectionRange

    val treeSelectionRange = toSelectionRange(tree.sourcePos)

    tree match
      case tpd.DefDef(name, paramss, tpt, rhs) =>
        // If source position is within a parameter list, add a selection range covering that whole list.
        val selectedParams =
          paramss
            .iterator
            .flatMap: // parameter list to a sourcePosition covering the whole list
              case Seq(param) => Some(param.sourcePos)
              case params @ Seq(head, tail*) =>
                val srcPos = head.sourcePos
                val lastSpan = tail.last.span
                Some(SourcePosition(srcPos.source, srcPos.span union lastSpan, srcPos.outer))
              case Seq() => None
            .find(_.contains(pos))
            .map(toSelectionRange)
        selectedParams ++ Seq(treeSelectionRange)
      case _ => Seq(treeSelectionRange)

  private def setParent(
      child: SelectionRange,
      parent: SelectionRange
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
