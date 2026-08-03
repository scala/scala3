package dotty.tools.dotc.semanticdb

import java.util.regex.Pattern
import java.io.ByteArrayOutputStream
import java.nio.file.*
import java.nio.charset.StandardCharsets
import java.util.stream.Collectors
import java.util.Comparator
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import javax.tools.ToolProvider
import org.junit.Assert.*
import org.junit.Test
import org.junit.experimental.categories.Category
import dotty.BootstrappedOnlyTests
import dotty.tools.dotc.Main
import dotty.tools.dotc.semanticdb
import dotty.tools.dotc.semanticdb.Scala3.given
import dotty.tools.dotc.semanticdb.internal.SemanticdbOutputStream
import dotty.tools.dotc.util.SourceFile

@main def updateExpect =
  SemanticdbTests().runExpectTest(updateExpectFiles = true)

/** Useful for printing semanticdb metac output for one file
 *
 *  @param root the output directory containing semanticdb output,
 *  only 1 semanticdb file should be present
 *  @param source the single source file producing the semanticdb
 */
@main def metac(root: String, source: String): Unit =
  val rootSrc = Paths.get(root)
  val sourceSrc = Paths.get(source)
  val semanticFile = FileSystems.getDefault.getPathMatcher("glob:**.semanticdb")
  def inputFile(): Path =
    val ls = Files.walk(rootSrc.resolve("META-INF").resolve("semanticdb"))
    val files =
      try ls.filter(p => semanticFile.matches(p)).collect(Collectors.toList).asScala
      finally ls.close()
    require(files.sizeCompare(1) == 0, s"No semanticdb files! $rootSrc")
    files.head
  val metacSb: StringBuilder = StringBuilder(5000)
  val semanticdbPath = inputFile()
  val doc = SemanticdbTests.loadTextDocumentUnsafe(sourceSrc.toAbsolutePath, semanticdbPath)
  SemanticdbTests.metac(doc, Paths.get(doc.uri))(using metacSb)
  Files.write(rootSrc.resolve("metac.expect"), metacSb.toString.getBytes(StandardCharsets.UTF_8))


@Category(Array(classOf[BootstrappedOnlyTests]))
class SemanticdbTests:
  val javaFile: PathMatcher = FileSystems.getDefault.getPathMatcher("glob:**.java")
  val scalaFile: PathMatcher = FileSystems.getDefault.getPathMatcher("glob:**.scala")
  val expectFile: PathMatcher = FileSystems.getDefault.getPathMatcher("glob:**.expect.scala")
  val rootSrc: Path = Paths.get(System.getProperty("dotty.tools.dotc.semanticdb.test"))
  val expectSrc: Path = rootSrc.resolve("expect")
  val javaRoot: Path = rootSrc.resolve("javacp")
  val metacExpectFile: Path = rootSrc.resolve("metac.expect")

  @Category(Array(classOf[dotty.SlowTests]))
  @Test def expectTests: Unit = if (!scala.util.Properties.isWin) runExpectTest(updateExpectFiles = false)

  def runExpectTest(updateExpectFiles: Boolean): Unit =
    val target = generateSemanticdb()
    val errors = mutable.ArrayBuffer.empty[Path]
    val metacSb: StringBuilder = StringBuilder(5000)
    def collectErrorOrUpdate(expectPath: Path, obtained: String) =
      if updateExpectFiles then
        Files.write(expectPath, obtained.getBytes(StandardCharsets.UTF_8))
        println("updated: " + expectPath)
      else
        val expected = new String(Files.readAllBytes(expectPath), StandardCharsets.UTF_8)
        val expectName = expectPath.getFileName
        val relExpect = rootSrc.relativize(expectPath)
        if expected.trim != obtained.trim then
          Files.write(expectPath.resolveSibling("" + expectName + ".out"), obtained.getBytes(StandardCharsets.UTF_8))
          errors += expectPath
    for source <- inputFiles().sorted do
      val filename = source.getFileName.toString
      val relpath = expectSrc.relativize(source)
      val semanticdbPath = target
        .resolve("META-INF")
        .resolve("semanticdb")
        .resolve(relpath)
        .resolveSibling(filename + ".semanticdb")
      val expectPath = source.resolveSibling(filename.replace(".scala", ".expect.scala"))
      val doc = SemanticdbTests.loadTextDocument(source, relpath, semanticdbPath)
      SemanticdbTests.metac(doc, rootSrc.relativize(source))(using metacSb)
      val obtained = trimTrailingWhitespace(SemanticdbTests.printTextDocument(doc))
      collectErrorOrUpdate(expectPath, obtained)
    collectErrorOrUpdate(metacExpectFile, metacSb.toString)
    for expect <- errors do
      def red(msg: String) = Console.RED + msg + Console.RESET
      def blue(msg: String) = Console.BLUE + msg + Console.RESET
      println(s"""[${red("error")}] check file ${blue(expect.toString)} does not match generated.
      |If you meant to make a change, replace the expect file by:
      |  mv ${expect.resolveSibling("" + expect.getFileName + ".out")} $expect
      |inspect with:
      |  diff $expect ${expect.resolveSibling("" + expect.getFileName + ".out")}
      |Or else update all expect files with
      |  sbt 'scala3-compiler-bootstrapped/Test/runMain dotty.tools.dotc.semanticdb.updateExpect'""".stripMargin)
    Files.walk(target).sorted(Comparator.reverseOrder).forEach(Files.delete)
    if errors.nonEmpty then
      fail(s"${errors.size} errors in expect test.")

  def trimTrailingWhitespace(s: String): String =
    Pattern.compile(" +$", Pattern.MULTILINE).matcher(s).replaceAll("")

  def inputFiles(): List[Path] =
    val ls = Files.walk(expectSrc)
    val files =
      try ls.filter(p => scalaFile.matches(p) && !expectFile.matches(p)).collect(Collectors.toList).asScala
      finally ls.close()
    require(files.nonEmpty, s"No input files! $expectSrc")
    files.toList

  def javaFiles(): List[Path] =
    val ls = Files.walk(javaRoot)
    val files =
      try ls.filter(p => javaFile.matches(p)).collect(Collectors.toList).asScala
      finally ls.close()
    require(files.nonEmpty, s"No input files! $expectSrc")
    files.toList

  def generateSemanticdb(): Path =
    val target = Files.createTempDirectory("semanticdb")
    val javaArgs = Array("-d", target.toString) ++ javaFiles().map(_.toString)
    val javac = ToolProvider.getSystemJavaCompiler
    val exitJava = javac.run(null, null, null, javaArgs*)
    assert(exitJava == 0, "java compiler has errors")
    val args = Array(
      "-Xsemanticdb",
      "-d", target.toString,
      "-feature",
      "-deprecation",
      // "-Ydebug-flags",
      // "-Vprint:extractSemanticDB",
      "-sourceroot", expectSrc.toString,
      "-classpath", target.toString,
      "-Xignore-scala2-macros",
      "-usejavacp",
      "-Wunused:all",
      "-Yreporter:dotty.tools.dotc.reporting.Reporter$SilentReporter",
    ) ++ inputFiles().map(_.toString)
    val exit = Main.process(args)
    assertFalse(s"dotc errors: ${exit.errorCount}", exit.hasErrors)
    target

end SemanticdbTests

object SemanticdbTests:
  def printTextDocument(doc: TextDocument): String =
    val byteStream = new ByteArrayOutputStream()
    val out = SemanticdbOutputStream.newInstance(byteStream)
    doc.writeTo(out)
    out.flush()
    DocumentPrinter.textDocumentPrettyPrint(byteStream.toByteArray.nn)
  end printTextDocument


  /** Load SemanticDB TextDocument for a single Scala source file
   *
   * @param scalaAbsolutePath      Absolute path to a Scala source file.
   * @param scalaRelativePath      scalaAbsolutePath relativized by the sourceroot.
   * @param semanticdbAbsolutePath Absolute path to the SemanticDB file.
   */
  def loadTextDocument(
                        scalaAbsolutePath: Path,
                        scalaRelativePath: Path,
                        semanticdbAbsolutePath: Path
                      ): TextDocument =
    val reluri = Tools.mkURIstring(scalaRelativePath)
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
