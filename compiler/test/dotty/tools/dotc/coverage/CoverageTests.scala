package dotty.tools.dotc.coverage

import org.junit.Test
import org.junit.Assert.*
import org.junit.experimental.categories.Category

import dotty.BootstrappedOnlyTests
import dotty.tools.dotc.Main

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.FileSystems
import java.nio.file.Paths
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.StandardCopyOption

@main def updateExpect =
  CoverageTests().runExpectTest(updateCheckfiles = true)

@Category(Array(classOf[BootstrappedOnlyTests]))
class CoverageTests:

  private val scalaFile = FileSystems.getDefault.nn.getPathMatcher("glob:**.scala").nn
  private val rootSrc = Paths.get(System.getProperty("dotty.tools.dotc.coverage.test")).nn
  private val expectDir = rootSrc.resolve("expect").nn

  @Category(Array(classOf[dotty.SlowTests]))
  @Test def expectTests: Unit =
    runExpectTest(dotty.Properties.testsUpdateCheckfile)

  /** Runs the tests */
  def runExpectTest(updateCheckfiles: Boolean): Unit =
    val sourceRoot = if updateCheckfiles then "../" else "."

    Files.walk(expectDir).nn.filter(scalaFile.matches).nn.forEach(p => {
      val path = p.nn
      val fileName = path.getFileName.nn.toString.nn.stripSuffix(".scala")
      val targetDir = computeCoverageInTmp(Seq(path), sourceRoot).nn
      val targetFile = targetDir.resolve(s"scoverage.coverage").nn
      val expectFile = expectDir.resolve(s"$fileName.scoverage.check").nn

      if updateCheckfiles then
        Files.copy(targetFile, expectFile, StandardCopyOption.REPLACE_EXISTING)
      else
        val expected = new String(Files.readAllBytes(expectFile), UTF_8)
        val obtained = new String(Files.readAllBytes(targetFile), UTF_8)
        assertEquals(expected, obtained)

    })

  /** Generates the coverage report for the given input file, in a temporary directory. */
  def computeCoverageInTmp(inputFiles: Seq[Path], sourceRoot: String): Path =
    val target = Files.createTempDirectory("coverage")
    val args = Array(
      "-Ycheck:instrumentCoverage",
      "-coverage-out",
      target.toString,
      "-coverage-sourceroot",
      sourceRoot,
      "-usejavacp"
    ) ++ inputFiles.map(_.toString)
    val exit = Main.process(args)
    assertFalse(s"Compilation failed, ${exit.errorCount} errors", exit.hasErrors)
    target.nn
