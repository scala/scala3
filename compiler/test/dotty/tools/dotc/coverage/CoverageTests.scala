package dotty.tools.dotc.coverage

import java.util.stream.Collectors
import org.junit.Assert._
import dotty.BootstrappedOnlyTests
import org.junit.experimental.categories.Category
import org.junit.Test
import java.nio.file.Files
import dotty.tools.dotc.Main
import scala.jdk.CollectionConverters._

import java.io.File
import java.nio.file.Path
import java.nio.file.FileSystems
import java.nio.file.Paths
import java.nio.charset.StandardCharsets
import scala.io.Source
import java.io.BufferedOutputStream
import java.io.FileOutputStream

@main def updateExpect =
  CoverageTests().runExpectTest(updateExpectFiles = true)

@Category(Array(classOf[BootstrappedOnlyTests]))
class CoverageTests {

  val scalaFile = FileSystems.getDefault.getPathMatcher("glob:**.scala")
  val rootSrc = Paths.get(System.getProperty("dotty.tools.dotc.coverage.test"))
  val expectSrc = rootSrc.resolve("expect")

  @Category(Array(classOf[dotty.SlowTests]))
  @Test def expectTests: Unit =
    if (!scala.util.Properties.isWin) runExpectTest(updateExpectFiles = false)

  def runExpectTest(updateExpectFiles: Boolean): Unit = {
    val target = generateCoverage(updateExpectFiles)
    val input = Source.fromFile(new File(target.toString, "scoverage.coverage"))
    val expectFile = new File(expectSrc.resolveSibling("scoverage.coverage.expect").toUri)

    if (updateExpectFiles) {
      val outputStream = new BufferedOutputStream(new FileOutputStream(expectFile))
      try {
        input.foreach(outputStream.write(_))
      } finally outputStream.close
    } else {
      val expected = new String(Files.readAllBytes(expectFile.toPath), StandardCharsets.UTF_8)
      val obtained = input.mkString

      assertEquals(expected, obtained)
    }
  }

  def inputFiles(): List[Path] = {
    val ls = Files.walk(expectSrc)
    val files =
      try ls.filter(p => scalaFile.matches(p)).collect(Collectors.toList).asScala
      finally ls.close()

    files.toList
  }

  def generateCoverage(updateExpectFiles: Boolean): Path = {
    val target = Files.createTempDirectory("coverage")
    val args = Array(
      "-coverage",
      target.toString,
      "-coverage-sourceroot",
      if (updateExpectFiles) "../" else ".",
      "-usejavacp"
    ) ++ inputFiles().map(_.toString)
    val exit = Main.process(args)
    assertFalse(s"dotc errors: ${exit.errorCount}", exit.hasErrors)
    target
  }
}
