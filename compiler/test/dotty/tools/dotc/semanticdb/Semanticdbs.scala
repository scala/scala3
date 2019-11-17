package dotty.tools.dotc.semanticdb

import java.nio.file._
import java.nio.charset.StandardCharsets
import scala.collection.JavaConverters._
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.semanticdb.Scala.{_, given}

object Semanticdbs with

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
    val reluri = scalaRelativePath.toString
    val sdocs = parseTextDocuments(semanticdbAbsolutePath)
    sdocs.documents.find(_.uri == reluri) match
    case None => throw new NoSuchElementException(reluri)
    case Some(document) =>
      val text = new String(Files.readAllBytes(scalaAbsolutePath), StandardCharsets.UTF_8)
      // Assert the SemanticDB payload is in-sync with the contents of the Scala file on disk.
      val md5FingerprintOnDisk = internal.MD5.compute(text)
      if document.md5 != md5FingerprintOnDisk
        throw new IllegalArgumentException("stale semanticdb: " + reluri)
      else
        // Update text document to include full text contents of the file.
        document.copy(text = text)
  end loadTextDocument

  /** Parses SemanticDB text documents from an absolute path to a `*.semanticdb` file. */
  def parseTextDocuments(path: Path): TextDocuments =
    val bytes = Files.readAllBytes(path) // NOTE: a semanticdb file is a TextDocuments message, not TextDocument
    TextDocuments.parseFrom(bytes)

  /** Prettyprint a text document with symbol occurrences next to each resolved identifier.
   *
   * Useful for testing purposes to ensure that SymbolOccurrence values make sense and are correct.
   * Example output (NOTE, slightly modified to avoid "unclosed comment" errors):
   * {{{
   *   class Example *example/Example#*  {
   *     val a *example/Example#a.* : String *scala/Predef.String#* = "1"
   *   }
   * }}}
   **/
  def printTextDocument(doc: TextDocument): String =
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
      if !occ.symbol.isPackage && !isPrimaryConstructor
        sb.append(doc.text.substring(offset, end))
        sb.append("/*")
          .append(if (occ.role.isDefinition) "<-" else "->")
          .append(occ.symbol.replace("/", "::"))
          .append("*/")
        offset = end
    sb.append(doc.text.substring(offset))
    sb.toString
  end printTextDocument

  def schemaString(schema: Schema) =
    import Schema._
    schema match
    case SEMANTICDB3     => "SemanticDB v3"
    case SEMANTICDB4     => "SemanticDB v4"
    case LEGACY          => "SemanticDB legacy"
    case Unrecognized(_) => "unknown"
  end schemaString

  def languageString(language: Language) =
    import Language._
    language match
    case SCALA                              => "Scala"
    case JAVA                               => "Java"
    case UNKNOWN_LANGUAGE | Unrecognized(_) => "unknown"
  end languageString

  def processSymbol(info: SymbolInformation)(given sb: StringBuilder): Unit =
    import SymbolInformation.Kind._
    sb.append(info.symbol).append(" => ")
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

  def processOccurrence(occ: SymbolOccurrence)(given sb: StringBuilder, sourceFile: SourceFile): Unit =
    occ.range match
    case Some(range) =>
      sb.append('[')
        .append(range.startLine).append(':').append(range.startCharacter)
        .append("..")
        .append(range.endLine).append(':').append(range.endCharacter)
        .append("):")
      if range.endLine == range.startLine
      && range.startCharacter != range.endCharacter
      && !(occ.symbol.isConstructor && occ.role.isDefinition)
        val line = sourceFile.lineContent(sourceFile.lineToOffset(range.startLine))
        sb.append(" ").append(line.substring(range.startCharacter, range.endCharacter))
    case _ =>
      sb.append("[):")
    end match
    sb.append(if occ.role.isReference then " -> " else " <- ").append(occ.symbol).nl
  end processOccurrence

  def metac(doc: TextDocument, realPath: Path)(given sb: StringBuilder): StringBuilder =
    val realURI = realPath.toString
    given SourceFile = SourceFile.virtual(doc.uri, doc.text)
    sb.append(realURI).nl
    sb.append("_" * realURI.length).nl
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

  /** Sort symbol occurrences by their start position. */
  given buildOccurrenceOrdering: Ordering[SymbolOccurrence] = (x, y) =>
    x.range -> y.range match
    case None -> _ | _ -> None => 0
    case Some(a) -> Some(b) =>
      val byLine = Integer.compare(a.startLine, b.startLine)
      if (byLine != 0)
        byLine
      else // byCharacter
        Integer.compare(a.startCharacter, b.startCharacter)
  end buildOccurrenceOrdering

  given Ordering[SymbolInformation] = Ordering.by[SymbolInformation, String](_.symbol)(IdentifierOrdering())

  /**
    * A comparator for identifier like "Predef" or "Function10".
    *
    * Differences from the default string comparator:
    * - works with CharSequences like compiler `Name`
    * - orders numbers by their numerical value instead of lexicographical
    *   - Good: `Function1`, `Function2`,  `Function10`
    *   - Bad:  `Function1`, `Function10`, `Function2`
    *
    * taken from https://github.com/scalameta/scalameta/blob/master/semanticdb/metap/src/main/scala/scala/meta/internal/metap/IdentifierOrdering.scala
    */
  class IdentifierOrdering[T <: CharSequence] extends Ordering[T] with

    override def compare(o1: T, o2: T): Int =
      val len = math.min(o1.length(), o2.length())
      var i = 0
      while i < len do
        val a = o1.charAt(i)
        val b = o2.charAt(i)
        if a.isDigit && b.isDigit
          val byDigit = Integer.compare(toDigit(o1, i), toDigit(o2, i))
          if (byDigit != 0) return byDigit
          else
            i = seekNonDigit(o1, i)
        else
          val result = Character.compare(a, b)
          if result != 0
            return result
          i += 1
      end while
      Integer.compare(o1.length(), o2.length())
    end compare

    private def seekNonDigit(cs: T, i: Int): Int =
      var curr = i
      while curr < cs.length && cs.charAt(curr).isDigit do
        curr += 1
      curr
    end seekNonDigit

    private def toDigit(cs: T, i: Int): Int =
      val digit = cs.subSequence(i, seekNonDigit(cs, i))
      Integer.parseUnsignedInt(digit.toString)
    end toDigit

  end IdentifierOrdering

  inline def (sb: StringBuilder) nl = sb.append(System.lineSeparator)
