package dotty.tools.dotc.semanticdb

import java.net.URLClassLoader
import java.util.regex.Pattern
import java.io.File
import java.nio.file._
import java.nio.charset.StandardCharsets
import java.util.stream.Collectors
import java.util.Comparator
import scala.util.control.NonFatal
import scala.collection.mutable
import scala.jdk.CollectionConverters._

import org.junit.Assert._
import org.junit.Test
import org.junit.experimental.categories.Category

import dotty.BootstrappedOnlyTests
import dotty.tools.dotc.Main
import dotty.tools.dotc.semanticdb.DiffAssertions._
import dotty.tools.dotc.semanticdb.Scala3.given
import dotty.tools.dotc.util.SourceFile

@main def updateExpect =
  SemanticdbTests().runExpectTest(updateExpectFiles = true)

@Category(Array(classOf[BootstrappedOnlyTests]))
class SemanticdbTests with
  val scalaFile = FileSystems.getDefault.getPathMatcher("glob:**.scala")
  val expectFile = FileSystems.getDefault.getPathMatcher("glob:**.expect.scala")
  // val semanticdbFile = FileSystems.getDefault.getPathMatcher("glob:**.scala.semanticdb")
  val rootSrc = Paths.get(System.getProperty("dotty.tools.dotc.semanticdb.test"))
  val expectSrc = rootSrc.resolve("expect")
  val metacExpectFile = rootSrc.resolve("metac.expect")

  @Category(Array(classOf[dotty.SlowTests]))
  @Test def expectTests: Unit = runExpectTest(updateExpectFiles = false)

  def runExpectTest(updateExpectFiles: Boolean): Unit =
    val target = generateSemanticdb()
    val errors = mutable.ArrayBuffer.empty[Path]
    given metacSb: StringBuilder = StringBuilder(5000)
    for source <- inputFiles().sorted do
      val filename = source.getFileName.toString
      val relpath = expectSrc.relativize(source)
      val semanticdbPath = target
        .resolve("META-INF")
        .resolve("semanticdb")
        .resolve(relpath)
        .resolveSibling(filename + ".semanticdb")
      val expectPath = source.resolveSibling(filename.replaceAllLiterally(".scala", ".expect.scala"))
      val doc = Tools.loadTextDocument(source, relpath, semanticdbPath)
      Tools.metac(doc, rootSrc.relativize(source))(given metacSb)
      val obtained = trimTrailingWhitespace(SemanticdbTests.printTextDocument(doc))
      if updateExpectFiles
        Files.write(expectPath, obtained.getBytes(StandardCharsets.UTF_8))
        println("updated: " + expectPath)
      else
        val expected = new String(Files.readAllBytes(expectPath), StandardCharsets.UTF_8)
        val expectName = expectPath.getFileName
        val relExpect = rootSrc.relativize(expectPath)
        collectFailingDiff(expected, obtained, s"a/$relExpect", s"b/$relExpect") {
          Files.write(expectPath.resolveSibling("" + expectName + ".out"), obtained.getBytes(StandardCharsets.UTF_8))
          errors += expectPath
        }
    if updateExpectFiles then
      Files.write(metacExpectFile, metacSb.toString.getBytes)
      println("updated: " + metacExpectFile)
    else
      val expected = new String(Files.readAllBytes(metacExpectFile), StandardCharsets.UTF_8)
      val expectName = metacExpectFile.getFileName
      val relExpect = rootSrc.relativize(metacExpectFile)
      val obtained = metacSb.toString
      def writeOut =
      collectFailingDiff(expected, obtained, s"a/$relExpect", s"b/$relExpect") {
        Files.write(metacExpectFile.resolveSibling("" + expectName + ".out"), obtained.getBytes(StandardCharsets.UTF_8));
        errors += metacExpectFile
      }
    errors.foreach { expect =>
      def red(msg: String) = Console.RED + msg + Console.RESET
      def blue(msg: String) = Console.BLUE + msg + Console.RESET
      println(s"""[${red("error")}] check file ${blue(expect.toString)} does not match generated.
      |If you meant to make a change, replace the expect file by:
      |  mv ${expect.resolveSibling("" + expect.getFileName + ".out")} $expect
      |Or else update all expect files with
      |  sbt 'dotty-compiler-bootstrapped/test:runMain dotty.tools.dotc.semanticdb.updateExpect'""".stripMargin)
    }
    Files.walk(target).sorted(Comparator.reverseOrder).forEach(Files.delete)
    if errors.nonEmpty
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

  def generateSemanticdb(): Path =
    val target = Files.createTempDirectory("semanticdb")
    val args = Array(
      "-Ysemanticdb",
      "-d", target.toString,
      "-feature",
      "-deprecation",
      // "-Ydebug-flags",
      // "-Xprint:extractSemanticDB",
      "-sourceroot", expectSrc.toString,
      "-usejavacp",
    ) ++ inputFiles().map(_.toString)
    val exit = Main.process(args)
    assertFalse(s"dotc errors: ${exit.errorCount}", exit.hasErrors)
    target

end SemanticdbTests

object SemanticdbTests with
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

end SemanticdbTests
