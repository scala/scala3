package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.{CodeRange, PositionContext}

import scala.collection.JavaConverters._

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

  override def onMarker(marker: CodeMarker): Exec[Unit] = {
    val results = server.references(fix(marker.toReferenceParams(withDecl))).get()
    assert(results.size() == expected.size)
    results.asScala.zip(expected).foreach { case (loc, ref) => assert(loc == ref.toLocation, results) }
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeReferences(${range.show}, ${expected.map(_.show)}, $withDecl)"
}
