package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.{CodeRange, PositionContext}

import org.eclipse.lsp4j._

import PositionContext._

/**
 * An action requesting for the info shown when `range` is hovered.
 * This action corresponds to the `textDocument/hover` method of the Language Server Protocol.
 *
 * @param range The range of positions that should be hovered.
 * @param expected The expected result.
 */
class CodeHover(override val range: CodeRange, expected: String) extends ActionOnRange {

  override def onMarker(marker: CodeMarker): Exec[Unit] = {
    val result = server.hover(fix(marker.toTextDocumentPositionParams)).get()
    assert(result.getRange == null)
    if (expected == "") assert(result.getContents == null, "Expected null contents in " + result)
    else {
      assert(result.getContents.size() == 1, result)
      val content = result.getContents.get(0)
      assert(content.isLeft, "Expected left but was " + content)
      assert(content.getLeft == expected, s"Expected $expected but was ${content.getLeft}")
    }
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeHover(${range.show}, $expected)"
}
