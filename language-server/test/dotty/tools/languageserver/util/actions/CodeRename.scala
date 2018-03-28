package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.{CodeRange, PositionContext}

import scala.collection.JavaConverters._

/**
 * An action requesting for a rename of the symbol at `marker`.
 * This action corresponds to the `textDocument/rename` method of the Language Server Protocol.
 *
 * @param marker The positions where to test to rename.
 * @param newName The new name to give to the selected symbol.
 * @param expected The expected ranges that should be modified.
 */
class CodeRename(override val marker: CodeMarker,
                 newName: String,
                 expected: List[CodeRange]) extends ActionOnMarker {

  override def execute(): Exec[Unit] = {
    val results = server.rename(marker.toRenameParams(newName)).get()
    assert(results.getDocumentChanges == null, results)
    val editItems = results.getChanges.values().asScala.flatMap(_.asScala) // TODO use a Map
    assert(expected.forall { exp =>
      editItems.exists { editItem =>
        editItem.getNewText == newName &&
        editItem.getRange == exp.toRange
      }
    }, results)
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeRename(${marker.show}, $newName, ${expected.map(_.show)})"
}
