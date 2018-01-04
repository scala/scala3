package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.PositionContext
import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.server.TestFile

import scala.collection.JavaConverters._

class CodeCompletion(val marker: CodeMarker, completions: List[(String, String, String)]) extends ActionOnMarker {

  override def execute(): Exec[Unit] = {
    val res = server.completion(marker.toTextDocumentPositionParams).get()
    assert(res.isRight, res)
    val cList = res.getRight
    assert(!cList.isIncomplete, res)
    completions.foreach { completion =>
      assert(
        cList.getItems.asScala.exists(item =>
          completion == (item.getLabel, item.getKind.toString, item.getDetail)
        ),
        "Did not return completion for " + completion + "\n" + cList.getItems.asScala.toList
      )
    }
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeCompletion(${marker.show}, $completions)"
}
