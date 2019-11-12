package dotty.tools.dotc.semanticdb

import java.nio.file._
import java.nio.charset.StandardCharsets
import scala.collection.JavaConverters._
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.semanticdb.Scala.{_, given}

object Semanticdbs {

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
  ): TextDocument = {
    val reluri = scalaRelativePath.iterator.asScala.mkString("/")
    val sdocs = parseTextDocuments(semanticdbAbsolutePath)
    sdocs.documents.find(_.uri == reluri) match {
      case None => throw new NoSuchElementException(reluri)
      case Some(document) =>
        val text = new String(Files.readAllBytes(scalaAbsolutePath), StandardCharsets.UTF_8)
        // Assert the SemanticDB payload is in-sync with the contents of the Scala file on disk.
        val md5FingerprintOnDisk = internal.MD5.compute(text)
        if (document.md5 != md5FingerprintOnDisk) {
          throw new IllegalArgumentException("stale semanticdb: " + reluri)
        } else {
          // Update text document to include full text contents of the file.
          document.copy(text = text)
        }
    }
  }

  /** Parses SemanticDB text documents from an absolute path to a `*.semanticdb` file. */
  def parseTextDocuments(path: Path): TextDocuments = {
    // NOTE: a *.semanticdb file is of type TextDocuments, not TextDocument
    val bytes = Files.readAllBytes(path)
    TextDocuments.parseFrom(bytes)
  }


  /** Prettyprint a text document with symbol occurrences next to each resolved identifier.
   *
   * Useful for testing purposes to ensure that SymbolOccurrence values make sense and are correct.
   * Example output (NOTE, slightly modified to avoid "unclosed comment" errors):
   * {{{
   *   class Example *example/Example#*  {
   *     val a *example/Example#a.* : String *scala/Predef.String#*  = "1"
   *   }
   * }}}
   **/
  def printTextDocument(doc: TextDocument): String = {
    val symtab = doc.symbols.iterator.map(info => info.symbol -> info).toMap
    val sb = new StringBuilder
    val sourceFile = SourceFile.virtual(doc.uri, doc.text)
    implicit val occurrenceOrdering: Ordering[SymbolOccurrence] =
      buildOccurrenceOrdering(sourceFile)
    val occurrences = doc.occurrences.sorted
    var offset = 0
    occurrences.foreach { occ =>
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
          .append(if (occ.role.isDefinition) "<<=" else "=>>")
          .append(occ.symbol.replace('/', '.'))
          .append("*/")
        offset = end
    }
    sb.append(doc.text.substring(offset))
    sb.toString()
  }

  /** Sort symbol occurrences by their start position. */
  def buildOccurrenceOrdering(sourceFile: SourceFile): Ordering[SymbolOccurrence] = {
    new Ordering[SymbolOccurrence] {
      def rangeToTuple(r : Range): (Int, Int) = {
        val start = sourceFile.lineToOffset(r.startLine) + r.startCharacter
        val end = sourceFile.lineToOffset(r.endLine) + r.endCharacter
        (start, end)
      }

      override def compare(x: SymbolOccurrence, y: SymbolOccurrence): Int = {
        if (x.range.isEmpty) 0
        else if (y.range.isEmpty) 0
        else {
          val (as, ae) = rangeToTuple(x.range.get)
          val (bs, be) = rangeToTuple(y.range.get)
          val byStart = Integer.compare(as, bs)
          if (byStart != 0) {
            byStart
          } else {
            Integer.compare(ae, be)
          }
        }
      }
    }
  }
}
