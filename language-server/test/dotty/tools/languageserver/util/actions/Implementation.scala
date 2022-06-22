package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.{CodeRange, PositionContext}

import org.eclipse.lsp4j.Location

import org.junit.Assert.assertEquals

import scala.jdk.CollectionConverters._

/**
 * An action requesting the implementations of the symbol inside `range`.
 * This action corresponds to the `textDocument/implementation` method of the Language Server
 * Protocol.
 *
 * @param range    The range of position for which to request implementations.
 * @param expected The expected results.
 */
class Implementation(override val range: CodeRange, expected: List[CodeRange]) extends ActionOnRange {

  private implicit val LocationOrdering: Ordering[Location] = Ordering.by(_.toString)

  override def onMarker(marker: CodeMarker): Exec[Unit] = {
    val expectedLocations = expected.map(_.toLocation)
    val results: Seq[org.eclipse.lsp4j.Location] = server.implementation(marker.toTextDocumentPositionParams).get().asScala.toSeq

    assertEquals(expectedLocations.length, results.length)
    expectedLocations.sorted.zip(results.sorted).foreach {
      assertEquals(_, _)
    }
  }

  override def show: PositionContext.PosCtx[String] =
    s"Implementation(${range.show}, $expected)"
}
