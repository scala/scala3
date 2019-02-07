package dotty.semanticdb

import java.nio.file._
import java.nio.charset.StandardCharsets
import scala.meta.internal.{semanticdb => s}
import scala.collection.JavaConverters._
import dotty.tools.dotc.util.SourceFile

object Semanticdbs {
  private val buildSubFolder = "semanticdb/input/"
  /**
   * Utility to load SemanticDB for Scala source files.
   *
   * @param sourceroot The workspace root directory, by convention matches the directory of build.sbt
   * @param classpath The classpath for this project, can be a combination of jars and directories.
   *                  Matches the `fullClasspath` task key from sbt but can be only `classDirectory`
   *                  if you only care about reading SemanticDB files from a single project.
   */
  class Loader(sourceroot: Path, classpath: List[Path]) {
    private val META_INF = Paths.get("META-INF", "semanticdb").resolve(buildSubFolder)
    private val classLoader = new java.net.URLClassLoader(classpath.map(_.toUri.toURL).toArray)
    /** Returns a SemanticDB for a single Scala source file, if any. The path must be absolute. */
    def resolve(scalaAbsolutePath: Path): Option[s.TextDocument] = {
      val scalaRelativePath = sourceroot.relativize(scalaAbsolutePath)
      val filename = scalaRelativePath.getFileName.toString
      val semanticdbRelativePath = scalaRelativePath.resolveSibling(filename + ".semanticdb")
      val metaInfPath = META_INF.resolve(semanticdbRelativePath).toString
      Option(classLoader.findResource(metaInfPath)).map { url =>
        val semanticdbAbsolutePath = Paths.get(url.toURI)
        Semanticdbs.loadTextDocument(scalaAbsolutePath, scalaRelativePath, semanticdbAbsolutePath)
      }
    }
  }

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
  ): s.TextDocument = {
    val reluri = buildSubFolder + scalaRelativePath.iterator.asScala.mkString("/")
    val sdocs = parseTextDocuments(semanticdbAbsolutePath)
    sdocs.documents.find(_.uri == reluri) match {
      case None => throw new NoSuchElementException(reluri)
      case Some(document) =>
        val text = new String(Files.readAllBytes(scalaAbsolutePath), StandardCharsets.UTF_8)
        // Assert the SemanticDB payload is in-sync with the contents of the Scala file on disk.
        val md5FingerprintOnDisk = MD5.compute(text)
        if (document.md5 != md5FingerprintOnDisk) {
          throw new IllegalArgumentException("stale semanticdb: " + reluri)
        } else {
          // Update text document to include full text contents of the file.
          document.withText(text)
        }
    }
  }

  /** Parses SemanticDB text documents from an absolute path to a `*.semanticdb` file. */
  def parseTextDocuments(path: Path): s.TextDocuments = {
    // NOTE: a *.semanticdb file is of type s.TextDocuments, not s.TextDocument
    val in = Files.newInputStream(path)
    try s.TextDocuments.parseFrom(in)
    finally in.close()
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
  def printTextDocument(doc: s.TextDocument): String = {
    val sb = new StringBuilder
    val sourceFile = SourceFile.virtual(doc.uri, doc.text)
    implicit val occurrenceOrdering: Ordering[s.SymbolOccurrence] =
      buildOccurrenceOrdering(sourceFile)
    val occurrences = doc.occurrences.sorted
    var offset = 0
    occurrences.foreach { occ =>
      val range = occ.range.get
      val end = sourceFile.lineToOffset(range.endLine) + range.endCharacter
      sb.append(doc.text.substring(offset, end))
      sb.append(" /* ")
        .append(occ.symbol)
        .append(" */ ")
      offset = end
    }
    sb.append(doc.text.substring(offset))
    sb.toString()
  }

  /** Sort symbol occurrences by their start position. */
  def buildOccurrenceOrdering(sourceFile: SourceFile): Ordering[s.SymbolOccurrence] = {
    new Ordering[s.SymbolOccurrence] {
      def rangeToTuple(r : s.Range): (Int, Int) = {
        val start = sourceFile.lineToOffset(r.startLine) + r.startCharacter
        val end = sourceFile.lineToOffset(r.endLine) + r.endCharacter
        (start, end)
      }

      override def compare(x: s.SymbolOccurrence, y: s.SymbolOccurrence): Int = {
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
