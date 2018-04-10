package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.{CodeRange, PositionContext}

import org.junit.Assert.{assertEquals, assertNull, assertTrue}

/**
 * An action requesting for the info shown when `range` is hovered.
 * This action corresponds to the `textDocument/hover` method of the Language Server Protocol.
 *
 * @param range The range of positions that should be hovered.
 * @param expected The expected result.
 */
class CodeHover(override val range: CodeRange, expected: String) extends ActionOnRange {

  override def onMarker(marker: CodeMarker): Exec[Unit] = {
    val result = server.hover(marker.toTextDocumentPositionParams).get()
    assertNull(result.getRange)
    if (expected.isEmpty) assertNull(result.getContents)
    else {
      assertEquals(1, result.getContents.size)
      val content = result.getContents.get(0)
      assertTrue(content.isLeft)
      assertEquals(expected, content.getLeft)
    }
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeHover(${range.show}, $expected)"
}
