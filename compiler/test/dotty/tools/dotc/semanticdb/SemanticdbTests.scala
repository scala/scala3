package dotty.tools.dotc.semanticdb

import java.util.regex.Pattern
import scala.collection.mutable
import javax.tools.ToolProvider
import org.junit.Assert.*
import org.junit.Test
import org.junit.experimental.categories.Category
import dotty.BootstrappedOnlyTests
import dotty.tools.TestSources
import dotty.tools.dotc.Main
import dotty.tools.dotc.semanticdb
import dotty.tools.dotc.semanticdb.Scala3.given
import dotty.tools.dotc.util.SourceFile
import dotty.tools.nio.*
import dotty.tools.io.FileExtension

import scala.io.Codec

@main def updateExpect =
  SemanticdbTests().runExpectTest(updateExpectFiles = true)

/** Useful for printing semanticdb metac output for one file
 *
 *  @param root the output directory containing semanticdb output,
 *  only 1 semanticdb file should be present
 *  @param source the single source file producing the semanticdb
 */
@main def metac(root: String, source: String): Unit =
  val rootSrc = FileContainer.getOnDisk(root, "").get
  val sourceSrc = File.getOnDisk(source).get
  val semanticExt = FileExtension.from("semanticdb")
  def inputFile(): File =
    val files = rootSrc.getOrCreateContainer("META-INF").getOrCreateContainer("semanticdb").entries.collect {
      case f: File if f.extension == semanticExt => f
    }.toList
    require(files.sizeCompare(1) == 0, s"No semanticdb files! ${rootSrc.path}")
    files.head
  val metacSb: StringBuilder = StringBuilder(5000)
  val semanticdbPath = inputFile()
  val doc = SemanticdbTests.loadTextDocumentUnsafe(sourceSrc, semanticdbPath)
  SemanticdbTests.metac(doc, doc.uri)(using metacSb)
  rootSrc.getOrCreateFile("metac.expect").writeText(metacSb.toString, Codec.UTF8)


@Category(Array(classOf[BootstrappedOnlyTests]))
class SemanticdbTests:
  val expectExt = FileExtension.from("expect.scala")
  val rootSrc: FileContainer = TestSources.rootPath().getContainer("tests").get.getContainer("semanticdb").get
  val expectSrc: FileContainer = rootSrc.getOrCreateContainer("expect")
  val javaRoot: FileContainer = rootSrc.getOrCreateContainer("javacp")
  val metacExpectFile: File = rootSrc.getOrCreateFile("metac.expect")

  @Category(Array(classOf[dotty.SlowTests]))
  @Test def expectTests: Unit = if (!scala.util.Properties.isWin) runExpectTest(updateExpectFiles = false)

  def runExpectTest(updateExpectFiles: Boolean): Unit =
    val target = generateSemanticdb()
    val errors = mutable.ArrayBuffer.empty[File]
    val metacSb: StringBuilder = StringBuilder(5000)
    def collectErrorOrUpdate(expectPath: File, obtained: String) =
      if updateExpectFiles then
        expectPath.writeText(obtained, Codec.UTF8)
        println("updated: " + expectPath)
      else
        val expected = expectPath.readText(Codec.UTF8)
        val expectName = expectPath.name
        if expected.trim != obtained.trim then
          expectPath.parent.getOrCreateFile(expectName, FileExtension.from("out")).writeText(obtained, Codec.UTF8)
          errors += expectPath
    for source <- inputFiles().sortBy(_.path) do
      val filename = source.name
      val relativeSource = source.path.substring(expectSrc.path.length)
      val semanticdbPath = s"${target.path}${FileSystemEntry.separator}META-INF${FileSystemEntry.separator}semanticdb$relativeSource.semanticdb"
      val expectPath = source.parent.getOrCreateFile(source.nameWithoutExtension, expectExt)
      val doc = SemanticdbTests.loadTextDocument(expectSrc, source, File.getOrCreateOnDisk(semanticdbPath))
      SemanticdbTests.metac(doc, expectSrc.name + relativeSource)(using metacSb)
      val obtained = trimTrailingWhitespace(SemanticdbTests.printTextDocument(doc))
      collectErrorOrUpdate(expectPath, obtained)
    collectErrorOrUpdate(metacExpectFile, metacSb.toString)
    for expect <- errors do
      def red(msg: String) = Console.RED + msg + Console.RESET
      def blue(msg: String) = Console.BLUE + msg + Console.RESET
      val outPath = expect.parent.getOrCreateFile(expect.name, FileExtension.from("out")).path
      println(s"""[${red("error")}] check file ${blue(expect.path)} does not match generated.
      |If you meant to make a change, replace the expect file by:
      |  mv $outPath ${expect.path}
      |inspect with:
      |  diff ${expect.path} $outPath
      |Or else update all expect files with
      |  sbt 'scala3-compiler-bootstrapped/Test/runMain dotty.tools.dotc.semanticdb.updateExpect'""".stripMargin)
    target.deleteRecursively()
    if errors.nonEmpty then
      fail(s"${errors.size} errors in expect test.")

  def trimTrailingWhitespace(s: String): String =
    Pattern.compile(" +$", Pattern.MULTILINE).matcher(s).replaceAll("")

  def inputFiles(): List[File] =
    val files = expectSrc.recursiveEntries.collect {
      case f: File if f.extension == FileExtension.Scala && !f.nameWithoutExtension.endsWith(".expect") => f
    }.toList
    require(files.nonEmpty, s"No input files! ${expectSrc.path}")
    files

  def javaFiles(): List[File] =
    val files = javaRoot.recursiveEntries.collect {
      case f: File if f.extension == FileExtension.Java => f
    }.toList
    require(files.nonEmpty, s"No input files! ${javaRoot.path}")
    files

  def generateSemanticdb(): FileContainer =
    val target = FileContainer.createTemporaryOnDisk("semanticdb")
    val javaArgs = Array("-d", target.path) ++ javaFiles().map(_.path)
    val javac = ToolProvider.getSystemJavaCompiler
    val exitJava = javac.run(null, null, null, javaArgs*)
    assert(exitJava == 0, "java compiler has errors")
    val args = Array(
      "-Xsemanticdb",
      "-d", target.path,
      "-feature",
      "-deprecation",
      // "-Ydebug-flags",
      // "-Vprint:extractSemanticDB",
      "-sourceroot", expectSrc.path,
      "-classpath", target.path,
      "-Xignore-scala2-macros",
      "-usejavacp",
      "-Wunused:all",
      "-Yreporter:dotty.tools.dotc.reporting.Reporter$SilentReporter",
    ) ++ inputFiles().map(_.path)
    val exit = Main.process(args)
    assertFalse(s"dotc errors: ${exit.errorCount}", exit.hasErrors)
    target

end SemanticdbTests

object SemanticdbTests:
  def printTextDocument(doc: TextDocument): String =
    DocumentPrinter.textDocumentPrettyPrint(doc.toByteArray)
  end printTextDocument


  /** Load SemanticDB TextDocument for a single Scala source file
   *
   * @parma srcRoot        Source root for the Scala file
   * @param scalaPath      Scala file
   * @param semanticdbPath SemanticDB file.
   */
  def loadTextDocument( srcRoot: FileContainer,
                        scalaPath: File,
                        semanticdbAbsolutePath: File
                      ): TextDocument =
    val reluri = Tools.mkURIstring(java.nio.file.Path.of(scalaPath.path.substring(srcRoot.path.length)))
    val sdocs = parseTextDocuments(semanticdbAbsolutePath)
    sdocs.documents.find(_.uri == reluri) match
      case None => throw new NoSuchElementException(s"$reluri ($scalaPath) not found in ${sdocs.documents.map(_.uri).mkString("[", ", ", "]")} from $semanticdbAbsolutePath")
      case Some(document) =>
        val text = scalaPath.readText(Codec.UTF8)
        // Assert the SemanticDB payload is in-sync with the contents of the Scala file on disk.
        val md5FingerprintOnDisk = internal.MD5.compute(text)
        if document.md5 != md5FingerprintOnDisk then
          throw new IllegalArgumentException(s"stale semanticdb: $scalaPath")
        else
          // Update text document to include full text contents of the file.
          document.copy(text = text)
  end loadTextDocument

  def loadTextDocumentUnsafe(scalaAbsolutePath: File, semanticdbAbsolutePath: File): TextDocument =
    val docs = parseTextDocuments(semanticdbAbsolutePath).documents
    assert(docs.length == 1)
    docs.head.copy(text = scalaAbsolutePath.readText(Codec.UTF8))

  /** Parses SemanticDB text documents from an absolute path to a `*.semanticdb` file. */
  private def parseTextDocuments(path: File): TextDocuments =
    val bytes = path.readBytes() // NOTE: a semanticdb file is a TextDocuments message, not TextDocument
    TextDocuments.parseFrom(bytes)


  def metac(doc: TextDocument, realURI: String)(using sb: StringBuilder): StringBuilder =
    val symtab = PrinterSymtab.fromTextDocument(doc)
    val symPrinter = SymbolInformationPrinter(symtab)
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
    if doc.diagnostics.nonEmpty then
      sb.append("Diagnostics => ").append(doc.diagnostics.length).append(" entries").nl
    if doc.synthetics.nonEmpty then
      sb.append("Synthetics => ").append(doc.synthetics.length).append(" entries").nl
    sb.nl
    sb.append("Symbols:").nl
    doc.symbols.sorted.foreach(s => processSymbol(s, symPrinter))
    sb.nl
    sb.append("Occurrences:").nl
    doc.occurrences.sorted.foreach(processOccurrence)
    sb.nl
    if doc.diagnostics.nonEmpty then
      sb.append("Diagnostics:").nl
      doc.diagnostics.sorted.foreach(d => processDiag(d))
      sb.nl
    if doc.synthetics.nonEmpty then
      sb.append("Synthetics:").nl
      doc.synthetics.sorted.foreach(s => processSynth(s, synthPrinter))
      sb.nl
    sb
  end metac

  private def schemaString(schema: Schema) =
    import Schema.*
    schema match
      case SEMANTICDB3 => "SemanticDB v3"
      case SEMANTICDB4 => "SemanticDB v4"
      case LEGACY => "SemanticDB legacy"
      case Unrecognized(_) => "unknown"
  end schemaString

  private def languageString(language: Language) =
    import Language.*
    language match
      case SCALA => "Scala"
      case JAVA => "Java"
      case UNKNOWN_LANGUAGE | Unrecognized(_) => "unknown"
  end languageString

  private def processSymbol(info: SymbolInformation, printer: SymbolInformationPrinter)(using sb: StringBuilder): Unit =
    sb.append(printer.pprintSymbolInformation(info)).nl

  private def processSynth(synth: Synthetic, printer: SyntheticPrinter)(using sb: StringBuilder): Unit =
    sb.append(printer.pprint(synth)).nl

  private def processDiag(d: Diagnostic)(using sb: StringBuilder): Unit =
    d.range match
      case Some(range) => processRange(sb, range)
      case _ => sb.append("[):")
    sb.append(" ")
    d.severity match
      case Diagnostic.Severity.ERROR => sb.append("[error]")
      case Diagnostic.Severity.WARNING => sb.append("[warning]")
      case Diagnostic.Severity.INFORMATION => sb.append("[info]")
      case _ => sb.append("[unknown]")
    sb.append(" ")
    sb.append(d.message)
    sb.nl

  private def processOccurrence(occ: SymbolOccurrence)(using sb: StringBuilder, sourceFile: SourceFile): Unit =
    occ.range match
      case Some(range) =>
        processRange(sb, range)
        if range.endLine == range.startLine
          && range.startCharacter != range.endCharacter
          && !(occ.symbol.isConstructor && occ.role.isDefinition) then
          val line = sourceFile.lineContent(sourceFile.lineToOffset(range.startLine))
          assert(range.startCharacter <= line.length && range.endCharacter <= line.length,
            s"Line is only ${line.length} - start line was ${range.startLine} in source ${sourceFile.file.name}"
          )
          sb.append(" ").append(line.substring(range.startCharacter, range.endCharacter))
      case _ =>
        sb.append("[):")
    end match
    sb.append(if occ.role.isReference then " -> " else " <- ").append(occ.symbol).nl
  end processOccurrence

  extension (sb: StringBuilder)
    private inline def nl = sb.append(System.lineSeparator)

end SemanticdbTests
