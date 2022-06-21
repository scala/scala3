package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.{CodeRange, PositionContext}

import org.junit.Assert.{assertEquals, assertNull, assertTrue}

import scala.jdk.CollectionConverters._

/**
 * An action requesting for the info shown when `range` is hovered.
 * This action corresponds to the `textDocument/hover` method of the Language Server Protocol.
 *
 * @param range     The range of positions that should be hovered.
 * @param expected  None if no response is expected, the expected Markdown string otherwise.
 */
class CodeHover(override val range: CodeRange, expectedOpt: Option[String]) extends ActionOnRange {

  override def onMarker(marker: CodeMarker): Exec[Unit] = {
    val result = server.hover(marker.toTextDocumentPositionParams).get()
    expectedOpt match {
      case None =>
        assertNull(result)
      case Some(expected) =>
        assertNull(result.getRange)
        val contents = result.getContents.getRight
        assertEquals(contents.getKind, "markdown")
        assertEquals(expected, contents.getValue)
    }
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeHover(${range.show}, $expectedOpt)"
}
