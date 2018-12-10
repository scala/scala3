package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.{CodeRange, PositionContext}
import org.junit.Assert.{assertEquals, assertNotNull, assertNull, assertTrue}

import scala.collection.JavaConverters._

/**
 * An action that asks the language server whether a rename operation can be performed at
 * each of the position in `range`. If `success` is set, the language server is expected to
 * return a range of position corresponding to `range`. Otherwise, it is expected to return
 * `null`, indicating that the rename cannot be performed.
 *
 * This action corresponds to the `textDocument/prepareRename` method of the Language Server
 * Protocol.
 *
 * @param range   The range of positions to test.
 * @param success Whether the rename operation is expected to be possible at the given
 *                position.
 */
class PrepareRename(override val range: CodeRange,
                    success: Boolean) extends ActionOnRange {

  override def onMarker(marker: CodeMarker): Exec[Unit] = {
    val result = server.prepareRename(marker.toTextDocumentPositionParams).get()

    if (success) {
      assertNotNull(result)
      assertTrue(result.isLeft)
      assertEquals(result.getLeft, range.toRange)
    } else {
      assertNull(result)
    }
  }

  override def show: PositionContext.PosCtx[String] = {
    s"PrepareRename(${range.show}, success = $success)"
  }
}
