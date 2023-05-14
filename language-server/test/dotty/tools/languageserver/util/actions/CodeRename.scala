package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.{CodeRange, PositionContext}
import dotty.tools.languageserver.DottyLanguageServer.{RENAME_OVERRIDDEN, RENAME_NO_OVERRIDDEN}

import org.junit.Assert.{assertEquals, assertNull, fail}

import org.eclipse.lsp4j.{MessageActionItem, ShowMessageRequestParams}

import java.util.concurrent.CompletableFuture

import scala.jdk.CollectionConverters._

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
                 expected: Set[CodeRange],
                 withOverridden: Option[Boolean]) extends ActionOnMarker {

  private final val TIMEOUT_MS = 10000

  override def execute(): Exec[Unit] = {
    val query = server.rename(marker.toRenameParams(newName))

    withOverridden.foreach { includeOverridden =>
      var question: (ShowMessageRequestParams, CompletableFuture[MessageActionItem]) = null
      val startTime = System.currentTimeMillis()
      while {
        Thread.sleep(50)
        question = client.requests.get.headOption.orNull
        question == null && System.currentTimeMillis() - startTime < TIMEOUT_MS
      } do ()

      if (question == null) fail("The server didn't ask about overridden symbols.")

      val answerStr = if (includeOverridden) RENAME_OVERRIDDEN else RENAME_NO_OVERRIDDEN
      val action = question._1.getActions.asScala.find(_.getTitle == answerStr).get
      question._2.complete(action)
    }

    val results = query.get()

    val changes = results.getChanges.asScala.view.mapValues(_.asScala.toSet.map(ch => (ch.getNewText, ch.getRange))).toMap
    val expectedChanges = expected.groupBy(_.file.uri).view.mapValues(_.map(range => (newName, range.toRange))).toMap

    assertNull(results.getDocumentChanges)
    assertEquals(expectedChanges, changes)
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeRename(${marker.show}, $newName, ${expected.map(_.show)})"
}
