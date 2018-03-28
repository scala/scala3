package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.PositionContext
import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.server.TestFile

import org.eclipse.lsp4j.CompletionItemKind

import scala.collection.JavaConverters._

/**
 * An action requesting for code completion at `marker`, expecting `completions`.
 * This action corresponds to the `textDocument/completion` method of the Language Server Protocol.
 *
 * @param marker      The marker indicating the position where completion should be requested.
 * @param completions The expected results from the language server.
 */
class CodeCompletion(override val marker: CodeMarker,
                     completions: List[(String, CompletionItemKind, String)]) extends ActionOnMarker {

  override def execute(): Exec[Unit] = {
    val result = server.completion(marker.toTextDocumentPositionParams).get()
    assert(result.isRight, result)
    val cList = result.getRight
    assert(!cList.isIncomplete, result)
    completions.foreach { completion =>
      assert(
        cList.getItems.asScala.exists(item =>
          completion == (item.getLabel, item.getKind, item.getDetail)
        ),
        "Did not return completion for " + completion + "\n" + cList.getItems.asScala.toList
      )
    }
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeCompletion(${marker.show}, $completions)"
}
