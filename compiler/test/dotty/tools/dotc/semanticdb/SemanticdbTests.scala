package dotty.tools.dotc.semanticdb

import java.net.URLClassLoader
import java.util.regex.Pattern
import java.io.File
import java.nio.file._
import java.nio.charset.StandardCharsets
import java.util.stream.Collectors
import scala.util.control.NonFatal
import org.junit.Assert._
import org.junit.Test
import org.junit.experimental.categories.Category
import scala.collection.JavaConverters._
import dotty.tools.dotc.Main
import dotty.tools.dotc.semanticdb.DiffAssertions._


@main def updateExpect() =
  new SemanticdbTests().runExpectTest(updateExpectFiles = true)

class SemanticdbTests {
  val scalaFile = FileSystems.getDefault.getPathMatcher("glob:**.scala")
  val expectFile = FileSystems.getDefault.getPathMatcher("glob:**.expect.scala")
  val semanticdbFile = FileSystems.getDefault.getPathMatcher("glob:**.scala.semanticdb")
  val src = Paths.get(System.getProperty("user.dir")).resolve("tests").resolve("semanticdb")

  @Category(Array(classOf[dotty.SlowTests]))
  @Test def expectTests: Unit = runExpectTest(updateExpectFiles = false)

  def runExpectTest(updateExpectFiles: Boolean): Unit =
    val target = generateSemanticdb()
    inputFiles().foreach { source =>
      val filename = source.getFileName.toString
      val relpath = src.relativize(source)
      val semanticdbPath = target
        .resolve("META-INF")
        .resolve("semanticdb")
        .resolve(relpath)
        .resolveSibling(filename + ".semanticdb")
      val expectPath = source.resolveSibling(filename.replaceAllLiterally(".scala", ".expect.scala"))
      val doc = Semanticdbs.loadTextDocument(source, relpath, semanticdbPath)
      val obtained = trimTrailingWhitespace(Semanticdbs.printTextDocument(doc))
      if (updateExpectFiles)
        Files.write(expectPath, obtained.getBytes(StandardCharsets.UTF_8))
        println("updated: " + expectPath)
      else
        val expected = new String(Files.readAllBytes(expectPath), StandardCharsets.UTF_8)
        assertNoDiff(obtained, expected)
    }

  def trimTrailingWhitespace(s: String): String =
    Pattern.compile(" +$", Pattern.MULTILINE).matcher(s).replaceAll("")

  def inputFiles(): List[Path] =
    val ls = Files.walk(src)
    val files =
      try ls.filter(p => scalaFile.matches(p) && !expectFile.matches(p)).collect(Collectors.toList).asScala
      finally ls.close()
    require(files.nonEmpty, s"No input files! $src")
    files.toList

  def generateSemanticdb(): Path =
    val target = Files.createTempDirectory("semanticdb")
    val args = Array(
      "-Ysemanticdb",
      "-d", target.toString,
      "-sourceroot", src.toString,
      "-usejavacp",
    ) ++ inputFiles().map(_.toString)
    val exit = Main.process(args)
    if (exit.hasErrors)
      sys.error(s"dotc errors: ${exit.errorCount}")
    target

}