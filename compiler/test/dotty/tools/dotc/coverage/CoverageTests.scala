package dotty.tools.dotc.coverage

import org.junit.Test
import org.junit.AfterClass
import org.junit.Assert.*
import org.junit.experimental.categories.Category

import dotty.{BootstrappedOnlyTests, Properties}
import dotty.tools.vulpix.TestConfiguration.*
import dotty.tools.vulpix.*

import java.nio.file.{Files, FileSystems, Path, Paths, StandardCopyOption}
import scala.jdk.CollectionConverters.*
import scala.language.unsafeNulls
import dotty.tools.dotc.Main

@Category(Array(classOf[BootstrappedOnlyTests]))
class CoverageTests:
  import CoverageTests.*
  import CoverageTests.given

  private val scalaFile = FileSystems.getDefault.getPathMatcher("glob:**.scala")
  private val rootSrc = Paths.get(System.getProperty("dotty.tools.dotc.coverage.test"))
  private val expectDir = rootSrc.resolve("expect")

  @Test
  def checkInstrumentedCode(): Unit =
    given TestGroup = TestGroup("instrumentCoverage")
    val updateCheckfiles = dotty.Properties.testsUpdateCheckfile
    val sourceRoot = rootSrc.toString

    Files.walk(expectDir).filter(scalaFile.matches).forEach(p => {
      val path = p
      val fileName = path.getFileName.toString.stripSuffix(".scala")
      val targetDir = computeCoverageInTmp(path, sourceRoot)
      val targetFile = targetDir.resolve(s"scoverage.coverage")
      val expectFile = expectDir.resolve(s"$fileName.scoverage.check")

      if updateCheckfiles then
        Files.copy(targetFile, expectFile, StandardCopyOption.REPLACE_EXISTING)
      else
        val expected = Files.readAllLines(expectFile).asScala
        val obtained = Files.readAllLines(targetFile).asScala
        if expected != obtained then
          for ((exp, actual),i) <- expected.zip(obtained).filter(_ != _).zipWithIndex do
            Console.err.println(s"wrong line ${i+1}:")
            Console.err.println(s"  expected: $exp")
            Console.err.println(s"  actual  : $actual")
          fail(s"$targetFile differs from expected $expectFile")

    })

  /** Generates the coverage report for the given input file, in a temporary directory. */
  def computeCoverageInTmp(inputFile: Path, sourceRoot: String)(using TestGroup): Path =
    val target = Files.createTempDirectory("coverage")
    val options = defaultOptions.and("-Ycheck:instrumentCoverage", "-coverage-out", target.toString, "-coverage-sourceroot", sourceRoot)
    compileFile(inputFile.toString, options).checkCompile()
    target

object CoverageTests extends ParallelTesting:
  import scala.concurrent.duration.*

  def maxDuration = 30.seconds
  def numberOfSlaves = 1

  def safeMode = Properties.testsSafeMode
  def testFilter = Properties.testsFilter
  def isInteractive = SummaryReport.isInteractive
  def updateCheckFiles = Properties.testsUpdateCheckfile

  given summaryReport: SummaryReporting = SummaryReport()
  @AfterClass def tearDown(): Unit =
    super.cleanup()
    summaryReport.echoSummary()
