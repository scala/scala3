package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.{CodeRange, PositionContext}

import org.junit.Assert.{assertEquals, assertNull, assertTrue}

import scala.collection.JavaConverters._

/**
 * An action requesting for the info shown when `range` is hovered.
 * This action corresponds to the `textDocument/hover` method of the Language Server Protocol.
 *
 * @param range The range of positions that should be hovered.
 * @param expected The expected result.
 */
class CodeHover(override val range: CodeRange, expected: List[String]) extends ActionOnRange {

  override def onMarker(marker: CodeMarker): Exec[Unit] = {
    val result = server.hover(marker.toTextDocumentPositionParams).get()
    assertNull(result.getRange)
    if (expected.isEmpty) assertNull(result.getContents)
    else {
      assertEquals(expected.size, result.getContents.size)
      expected.zip(result.getContents.asScala).foreach { case (expected, actual) =>
        assertTrue(actual.isRight)
        assertEquals(expected, actual.getRight.getValue)
      }
    }
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeHover(${range.show}, $expected)"
}
