package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.{CodeRange, PositionContext}

import scala.collection.JavaConverters._

class CodeRename(val marker: CodeMarker, newName: String, expected: List[CodeRange]) extends ActionOnMarker {

  override def execute(): Exec[Unit] = {
    val res = server.rename(marker.toRenameParams(newName)).get()
    assert(res.getDocumentChanges == null, res)
    val editItems = res.getChanges.values().asScala.flatMap(_.asScala) // TDO use a Map
    assert(expected.forall { exp =>
      editItems.exists { editItem =>
        editItem.getNewText == newName &&
        editItem.getRange == exp.toRange
      }
    }, res)
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeRename(${marker.show}, $newName, ${expected.map(_.show)})"
}
