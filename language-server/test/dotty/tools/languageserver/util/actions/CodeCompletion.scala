package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.PositionContext
import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.server.TestFile

import org.eclipse.lsp4j.CompletionItemKind

import scala.collection.JavaConverters._

/**
 * An action requesting for code completion at `marker`, expecting `expected`.
 * This action corresponds to the `textDocument/completion` method of the Language Server Protocol.
 *
 * @param marker    The marker indicating the position where completion should be requested.
 * @param expected The expected results from the language server.
 */
class CodeCompletion(override val marker: CodeMarker,
                     expected: Set[(String, CompletionItemKind, String)]) extends ActionOnMarker {

  override def execute(): Exec[Unit] = {
    val result = server.completion(marker.toTextDocumentPositionParams).get()
    assert(result.isRight, result)
    assert(!result.getRight.isIncomplete, s"Completion results were 'incomplete': $result")
    val completionResults = result.getRight.getItems.asScala.toSet.map { item =>
      (item.getLabel, item.getKind, item.getDetail)
    }
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeCompletion(${marker.show}, $expected)"
}
