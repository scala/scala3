package dotty.tools.languageserver.decompiler

import org.eclipse.lsp4j.TextDocumentIdentifier

// All case classes in this file should have zero-parameters secondary
// constructors to allow Gson to reflectively create instances on
// deserialization without relying on sun.misc.Unsafe.

/** The parameter for the `tasty/decompile` request. */
case class TastyDecompileParams(textDocument: TextDocumentIdentifier) {
  def this() = this(null)
}

/** The response to a `tasty/decompile` request. */
case class TastyDecompileResult(tastyTree: String = null, scala: String = null, error: Int = 0) {
  def this() = this(null, null, 0)
}

object TastyDecompileResult {
  val ErrorTastyVersion = 1
  val ErrorClassNotFound = 2
  val ErrorOther = -1
}