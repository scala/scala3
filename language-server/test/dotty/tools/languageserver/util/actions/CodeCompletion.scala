package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.PositionContext
import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.server.TestFile

import org.eclipse.lsp4j.CompletionItemKind
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}

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
    assertTrue(s"Completion results were not 'right': $result", result.isRight)
    assertFalse(s"Completion results were 'incomplete': $result", result.getRight.isIncomplete)
    val completionResults = result.getRight.getItems.asScala.toSet.map { item =>
      (item.getLabel, item.getKind, item.getDetail)
    }
    assertEquals(expected, completionResults)
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeCompletion(${marker.show}, $expected)"
}
