package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.PositionContext
import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.server.TestFile

import org.eclipse.lsp4j.{CompletionItem, CompletionItemKind}
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}

import scala.jdk.CollectionConverters._

/**
 * An action requesting for code completion at `marker`, expecting `expected`.
 * This action corresponds to the `textDocument/completion` method of the Language Server Protocol.
 *
 * @param marker       The marker indicating the position where completion should be requested.
 * @param checkResults A function that takes the results and verifies that they match
 *                     expectations.
 */
class CodeCompletion(override val marker: CodeMarker,
                     checkResults: Set[CompletionItem] => Unit)
    extends ActionOnMarker {

  override def execute(): Exec[Unit] = {
    val result = server.completion(marker.toCompletionParams).get()
    assertTrue(s"Completion results were not 'right': $result", result.isRight)
    assertFalse(s"Completion results were 'incomplete': $result", result.getRight.isIncomplete)
    val completionResults = result.getRight.getItems.asScala.toSet
    checkResults(completionResults)
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeCompletion(${marker.show}, $checkResults)"
}

object CodeCompletion {
  /** Extract the (label, kind, details) of each `CompletionItem`. */
  def simplifyResults(items: Set[CompletionItem]): Set[(String, CompletionItemKind, String)] =
    items.map(item => (item.getLabel, item.getKind, item.getDetail))
}
