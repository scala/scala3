package dotty.tools.languageserver.worksheet

import org.eclipse.lsp4j.{ Range, VersionedTextDocumentIdentifier }

// All case classes in this file should have zero-parameters secondary
// constructors to allow Gson to reflectively create instances on
// deserialization without relying on sun.misc.Unsafe.

/** The parameter for the `worksheet/run` request. */
case class WorksheetRunParams(textDocument: VersionedTextDocumentIdentifier) {
  def this() = this(null)
}

/** The response to a `worksheet/run` request. */
case class WorksheetRunResult(success: Boolean) {
  def this() = this(false)
}

/** The parameters to the `worksheet/publishOutput` notification. */
case class WorksheetRunOutput(textDocument: VersionedTextDocumentIdentifier, range: Range, content: String) {
  def this() = this(null, null, null)
}
