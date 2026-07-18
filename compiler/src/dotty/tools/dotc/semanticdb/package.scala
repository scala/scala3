package dotty.tools.dotc

import java.io.ByteArrayOutputStream
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.semanticdb.internal.MD5
import dotty.tools.dotc.semanticdb.internal.SemanticdbOutputStream

/**
 * Public API of the SemanticDB package.
 *
 * The `generated` package is NOT a public API, and neither is the `internal` package.
 * (However, in practice `internal` is used by the presentation-compiler to avoid depending on a protobuf runtime... see `replaceProtobuf` in project/Build.scala)
 */
package object semanticdb {
  /** Compiler pass to write SemanticDB information about code. */
  type ExtractSemanticInfo = ExtractSemanticDB.ExtractSemanticInfo

  /** Compiler pass to write SemanticDB information about compiler diagnostics. */
  type AppendDiagnostics = ExtractSemanticDB.AppendDiagnostics

  /** Serializes a SemanticDB text document as bytes for the given tree, path, and textual source code. */
  def textDocumentBytes(tree: tpd.Tree, path: String, sourceCode: String)(using Context): Array[Byte] =
    textDocument(tree, path, sourceCode).toByteArray

  /** Creates a SemanticDB text document for the given tree, path, and textual source code. */
  def textDocument(tree: tpd.Tree, path: String, sourceCode: String)(using Context): TextDocument =
    val extractor = ExtractSemanticDB.Extractor()
    extractor.traverse(tree)
    TextDocument(
      schema = Schema.SEMANTICDB4,
      language = Language.SCALA,
      uri = path,
      text = sourceCode,
      md5 = MD5.compute(sourceCode),
      symbols = extractor.symbolInfos.toList,
      occurrences = extractor.occurrences.toList
    )

  /** Gets SemanticDB symbols from the given name. */
  def symbolsFromName(sym: String)(using ctx: Context): List[Symbol] =
    SemanticSymbolBuilder.inverseSymbol(sym)

  /** Gets the SemanticDB name of the given symbol. */
  def symbolToName(sym: Symbol)(using Context): String =
    SemanticSymbolBuilder.symbolName(sym)
}