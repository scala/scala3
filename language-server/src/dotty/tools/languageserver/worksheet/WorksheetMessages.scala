package dotty.tools.languageserver.worksheet

import org.eclipse.lsp4j.VersionedTextDocumentIdentifier

// All case classes in this file should have zero-parameters secondary
// constructors to allow Gson to reflectively create instances on
// deserialization without relying on sun.misc.Unsafe.

/** The parameter for the `worksheet/exec` request. */
case class WorksheetExecParams(textDocument: VersionedTextDocumentIdentifier) {
  def this() = this(null)
}

/** The response to a `worksheet/exec` request. */
case class WorksheetExecResult(success: Boolean) {
  def this() = this(false)
}

/** The parameters to the `worksheet/publishOutput` notification. */
case class WorksheetExecOutput(textDocument: VersionedTextDocumentIdentifier, line: Int, content: String) {
  def this() = this(null, 0, null)
}
