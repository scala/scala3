package dotty.tools.dotc.semanticdb

import java.nio.file._
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters._
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.semanticdb.Scala3.given

object Tools:

  /** Converts a Path to a String that is URI encoded, without forcing absolute paths. */
  def mkURIstring(path: Path): String =
    // Calling `.toUri` on a relative path will convert it to absolute. Iteration through its parts instead preserves
    // the resulting URI as relative.
    val uriParts = for part <- path.asScala yield new java.net.URI(null, null, part.toString, null)
    uriParts.mkString("/")

  /** Load SemanticDB TextDocument for a single Scala source file
   *
   * @param scalaAbsolutePath Absolute path to a Scala source file.
   * @param scalaRelativePath scalaAbsolutePath relativized by the sourceroot.
   * @param semanticdbAbsolutePath Absolute path to the SemanticDB file.
   */
  def loadTextDocument(
    scalaAbsolutePath: Path,
    scalaRelativePath: Path,
    semanticdbAbsolutePath: Path
  ): TextDocument =
    val reluri  = mkURIstring(scalaRelativePath)
    val sdocs = parseTextDocuments(semanticdbAbsolutePath)
    sdocs.documents.find(_.uri == reluri) match
    case None => throw new NoSuchElementException(s"$scalaRelativePath")
    case Some(document) =>
      val text = new String(Files.readAllBytes(scalaAbsolutePath), StandardCharsets.UTF_8)
      // Assert the SemanticDB payload is in-sync with the contents of the Scala file on disk.
      val md5FingerprintOnDisk = internal.MD5.compute(text)
      if document.md5 != md5FingerprintOnDisk then
        throw new IllegalArgumentException(s"stale semanticdb: $scalaRelativePath")
      else
        // Update text document to include full text contents of the file.
        document.copy(text = text)
  end loadTextDocument

  def loadTextDocumentUnsafe(scalaAbsolutePath: Path, semanticdbAbsolutePath: Path): TextDocument =
    val docs = parseTextDocuments(semanticdbAbsolutePath).documents
    assert(docs.length == 1)
    docs.head.copy(text = new String(Files.readAllBytes(scalaAbsolutePath), StandardCharsets.UTF_8))

  /** Parses SemanticDB text documents from an absolute path to a `*.semanticdb` file. */
  private def parseTextDocuments(path: Path): TextDocuments =
    val bytes = Files.readAllBytes(path).nn // NOTE: a semanticdb file is a TextDocuments message, not TextDocument
    TextDocuments.parseFrom(bytes)

  def metac(doc: TextDocument, realPath: Path)(using sb: StringBuilder): StringBuilder =
    val symtab = PrinterSymtab.fromTextDocument(doc)
    val symPrinter = SymbolInformationPrinter(symtab)
    val realURI = realPath.toString
    given sourceFile: SourceFile = SourceFile.virtual(doc.uri, doc.text)
    val synthPrinter = SyntheticPrinter(symtab, sourceFile)
    sb.append(realURI).nl
    sb.append("-" * realURI.length).nl
    sb.nl
    sb.append("Summary:").nl
    sb.append("Schema => ").append(schemaString(doc.schema)).nl
    sb.append("Uri => ").append(doc.uri).nl
    sb.append("Text => empty").nl
    sb.append("Language => ").append(languageString(doc.language)).nl
    sb.append("Symbols => ").append(doc.symbols.length).append(" entries").nl
    sb.append("Occurrences => ").append(doc.occurrences.length).append(" entries").nl
    if doc.synthetics.nonEmpty then
      sb.append("Synthetics => ").append(doc.synthetics.length).append(" entries").nl
    sb.nl
    sb.append("Symbols:").nl
    doc.symbols.sorted.foreach(s => processSymbol(s, symPrinter))
    sb.nl
    sb.append("Occurrences:").nl
    doc.occurrences.sorted.foreach(processOccurrence)
    sb.nl
    if doc.synthetics.nonEmpty then
      sb.append("Synthetics:").nl
      doc.synthetics.sorted.foreach(s => processSynth(s, synthPrinter))
      sb.nl
    sb
  end metac

  private def schemaString(schema: Schema) =
    import Schema._
    schema match
    case SEMANTICDB3     => "SemanticDB v3"
    case SEMANTICDB4     => "SemanticDB v4"
    case LEGACY          => "SemanticDB legacy"
    case Unrecognized(_) => "unknown"
  end schemaString

  private def languageString(language: Language) =
    import Language._
    language match
    case SCALA                              => "Scala"
    case JAVA                               => "Java"
    case UNKNOWN_LANGUAGE | Unrecognized(_) => "unknown"
  end languageString

  private def processSymbol(info: SymbolInformation, printer: SymbolInformationPrinter)(using sb: StringBuilder): Unit =
    sb.append(printer.pprintSymbolInformation(info)).nl

  private def processSynth(synth: Synthetic, printer: SyntheticPrinter)(using sb: StringBuilder): Unit =
    sb.append(printer.pprint(synth)).nl

  private def processOccurrence(occ: SymbolOccurrence)(using sb: StringBuilder, sourceFile: SourceFile): Unit =
    occ.range match
    case Some(range) =>
      processRange(sb, range)
      if range.endLine == range.startLine
      && range.startCharacter != range.endCharacter
      && !(occ.symbol.isConstructor && occ.role.isDefinition) then
        val line = sourceFile.lineContent(sourceFile.lineToOffset(range.startLine))
        assert(range.startCharacter <= line.length && range.endCharacter <= line.length,
          s"Line is only ${line.length} - start line was ${range.startLine} in source ${sourceFile.name}"
        )
        sb.append(" ").append(line.substring(range.startCharacter, range.endCharacter))
    case _ =>
      sb.append("[):")
    end match
    sb.append(if occ.role.isReference then " -> " else " <- ").append(occ.symbol).nl
  end processOccurrence

  extension (sb: StringBuilder)
    private inline def nl = sb.append(System.lineSeparator)
