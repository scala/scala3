package dotty.tools.dotc.semanticdb

import dotty.tools.dotc.semanticdb.Scala3.{*, given}
import dotty.tools.dotc.util.SourceFile

object DocumentPrinter:
  /** Pretty-prints a SemanticDB `TextDocument` provided in its serialized form. Intended for testing and debugging. */
  def textDocumentPrettyPrint(bytes: Array[Byte]): String =
    DocumentPrinter.printTextDocument(TextDocument.parseFrom(bytes))
  
  /** Pretty-prints a text document with symbol occurrences next to each resolved identifier.
   *
   * Useful for testing purposes to ensure that SymbolOccurrence values make sense and are correct.
   * Example output (NOTE, slightly modified to avoid "unclosed comment" errors):
   * {{{
   *   class Example *example/Example#*  {
   *     val a *example/Example#a.* : String *scala/Predef.String#* = "1"
   *   }
   * }}}
   **/
  private def printTextDocument(doc: TextDocument): String =
    val symtab = doc.symbols.iterator.map(info => info.symbol -> info).toMap
    val sb = StringBuilder(1000)
    val sourceFile = SourceFile.virtual(doc.uri, doc.text)
    var offset = 0
    for occ <- doc.occurrences.sorted do
      val range = occ.range.get
      val end = math.max(
        offset,
        sourceFile.lineToOffset(range.endLine) + range.endCharacter
      )
      val isPrimaryConstructor =
        symtab.get(occ.symbol).exists(_.isPrimary)
      if !occ.symbol.isPackage && !isPrimaryConstructor then
        assert(end <= doc.text.length,
          s"doc is only ${doc.text.length} - offset=$offset, end=$end , symbol=${occ.symbol} in source ${sourceFile.name}")
        sb.append(doc.text.substring(offset, end))
        sb.append("/*")
          .append(if (occ.role.isDefinition) "<-" else "->")
          .append(occ.symbol.replace("/", "::"))
          .append("*/")
        offset = end
    assert(offset <= doc.text.length, s"absurd offset = $offset when doc is length ${doc.text.length}")
    sb.append(doc.text.substring(offset))
    sb.toString
  end printTextDocument
