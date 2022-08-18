package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.{CodeRange, PositionContext}

import org.junit.Assert.assertEquals

import org.eclipse.lsp4j.Location

import scala.jdk.CollectionConverters._

/**
 * An action requesting for all the references to the symbol in `range` within the workspace.
 * This action corresponds to the `textDocument/references` method of the Language Server Protocol.
 *
 * @param range The range of positions to test.
 * @param expected The expected results.
 * @param withDecl Whether the declaration of the current symbol should be included.
 */
class CodeReferences(override val range: CodeRange,
                     expected: List[CodeRange],
                     withDecl: Boolean) extends ActionOnRange {

  private implicit val LocationOrdering: Ordering[Location] = Ordering.by(_.toString)

  override def onMarker(marker: CodeMarker): Exec[Unit] = {
    val expectedLocations = expected.map(_.toLocation).sorted
    val results = server.references(marker.toReferenceParams(withDecl)).get().asScala.sorted

    assertEquals(expectedLocations, results)
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeReferences(${range.show}, ${expected.map(_.show)}, $withDecl)"
}
