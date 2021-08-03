package dotty.tools.dotc.semanticdb

import java.nio.file._
import java.nio.charset.StandardCharsets
import scala.collection.JavaConverters._
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.semanticdb.Scala3.{_, given}

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

  /** Parses SemanticDB text documents from an absolute path to a `*.semanticdb` file. */
  private def parseTextDocuments(path: Path): TextDocuments =
    val bytes = Files.readAllBytes(path) // NOTE: a semanticdb file is a TextDocuments message, not TextDocument
    TextDocuments.parseFrom(bytes)

  def metac(doc: TextDocument, realPath: Path)(using sb: StringBuilder): StringBuilder =
    val realURI = realPath.toString
    given SourceFile = SourceFile.virtual(doc.uri, doc.text)
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
    sb.nl
    sb.append("Symbols:").nl
    doc.symbols.sorted.foreach(processSymbol)
    sb.nl
    sb.append("Occurrences:").nl
    doc.occurrences.sorted.foreach(processOccurrence)
    sb.nl
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

  private def accessString(access: Access): String =
    access match
      case Access.Empty => ""
      case _: PublicAccess => ""
      case _: PrivateAccess => "private "
      case _: ProtectedAccess => "protected "
      case _: PrivateThisAccess => "private[this] "
      case _: ProtectedThisAccess => "protected[this] "
      case PrivateWithinAccess(ssym) =>
        s"private[${ssym}] "
      case ProtectedWithinAccess(ssym) =>
        s"protected[${ssym}] "


  private def processSymbol(info: SymbolInformation)(using sb: StringBuilder): Unit =
    import SymbolInformation.Kind._
    sb.append(info.symbol).append(" => ")
    sb.append(accessString(info.access))
    if info.isAbstract then sb.append("abstract ")
    if info.isFinal then sb.append("final ")
    if info.isSealed then sb.append("sealed ")
    if info.isImplicit then sb.append("implicit ")
    if info.isLazy then sb.append("lazy ")
    if info.isCase then sb.append("case ")
    if info.isCovariant then sb.append("covariant ")
    if info.isContravariant then sb.append("contravariant ")
    if info.isVal then sb.append("val ")
    if info.isVar then sb.append("var ")
    if info.isStatic then sb.append("static ")
    if info.isPrimary then sb.append("primary ")
    if info.isEnum then sb.append("enum ")
    if info.isDefault then sb.append("default ")
    if info.isGiven then sb.append("given ")
    if info.isInline then sb.append("inline ")
    if info.isOpen then sb.append("open ")
    if info.isTransparent then sb.append("transparent ")
    if info.isInfix then sb.append("infix ")
    if info.isOpaque then sb.append("opaque ")
    info.kind match
      case LOCAL => sb.append("local ")
      case FIELD => sb.append("field ")
      case METHOD => sb.append("method ")
      case CONSTRUCTOR => sb.append("ctor ")
      case MACRO => sb.append("macro ")
      case TYPE => sb.append("type ")
      case PARAMETER => sb.append("param ")
      case SELF_PARAMETER => sb.append("selfparam ")
      case TYPE_PARAMETER => sb.append("typeparam ")
      case OBJECT => sb.append("object ")
      case PACKAGE => sb.append("package ")
      case PACKAGE_OBJECT => sb.append("package object ")
      case CLASS => sb.append("class ")
      case TRAIT => sb.append("trait ")
      case INTERFACE => sb.append("interface ")
      case UNKNOWN_KIND | Unrecognized(_) => sb.append("unknown ")
    sb.append(info.displayName).nl
  end processSymbol

  private def processOccurrence(occ: SymbolOccurrence)(using sb: StringBuilder, sourceFile: SourceFile): Unit =
    occ.range match
    case Some(range) =>
      sb.append('[')
        .append(range.startLine).append(':').append(range.startCharacter)
        .append("..")
        .append(range.endLine).append(':').append(range.endCharacter)
        .append("):")
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
