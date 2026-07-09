package dotty.tools.dotc.coverage

import dotty.tools.TestSources
import org.junit.Test
import org.junit.AfterClass
import org.junit.Assert.*
import org.junit.experimental.categories.Category
import dotty.{BootstrappedOnlyTests, Properties}
import dotty.tools.vulpix.*
import dotty.tools.vulpix.TestConfiguration.*
import dotty.tools.dotc.reporting.TestReporter
import dotty.tools.nio.FileSystemEntry

import dotty.tools.nio.*
import dotty.tools.io.FileExtension

import scala.io.Codec

@Category(Array(classOf[BootstrappedOnlyTests]))
class CoverageTests:
  import CoverageTests.{*, given}

  private val rootSrc = TestSources.rootPath().getContainer("tests").get.getContainer("coverage").get

  @Test
  def checkCoverageStatements(): Unit =
    checkCoverageIn(rootSrc.getContainer("pos").get, false)

  @Test
  def checkInstrumentedRuns(): Unit =
    checkCoverageIn(rootSrc.getContainer("run").get, true)

  @Test
  def checkCoverageWarnings(): Unit =
    checkCoverageWarningsIn(rootSrc.getContainer("warn").get)

  def checkCoverageIn(dir: FileContainer, run: Boolean)(using TestGroup): Unit =
    /** Converts \\ (escaped \) to / on Windows, to make the tests pass without changing the serialization. */
    def fixWindowsPaths(lines: Iterable[String]): Iterable[String] =
      if FileSystemEntry.separator == '\\' then
        val escapedSep = "\\\\"
        lines.map(_.replace(escapedSep, "/"))
      else
        lines
    end fixWindowsPaths

    def runOnFileOrDir(p: FileSystemEntry): Boolean =
      (p.isInstanceOf[FileContainer] || p.path.endsWith(".scala"))
      && (p != dir)
      && (Properties.testsFilter.isEmpty || Properties.testsFilter.exists(p.path.contains))

    dir.entries.filter(runOnFileOrDir).foreach(path => {
      // measurement files only exist in the "run" category
      // as these are generated at runtime by the scala.runtime.coverage.Invoker
      val (targetDir, expectFile, expectMeasurementFile) = path match
        case d: FileContainer =>
          assert(d.recursiveEntries.collect { case f: File if f.extension == FileExtension.Scala => f }.nonEmpty, s"No scala files found in test directory: ${path.path}")
          val targetDir = computeCoverageInTmp(d, isDirectory = true, dir, run)
          (targetDir, d.getOrCreateFile("test.scoverage.check"), d.getFile("test.measurement.check"))
        case f: File =>
          val fileName = f.nameWithoutExtension
          val targetDir = computeCoverageInTmp(f, isDirectory = false, dir, run)
          (targetDir, f.parent.getOrCreateFile(s"$fileName.scoverage.check"), f.parent.getFile(s"$fileName.measurement.check"))

      val targetFile = targetDir.getFile("scoverage", FileExtension.from("coverage")).get

      if updateCheckFiles then
        targetFile.copyTo(expectFile)
      else
        val expected = fixWindowsPaths(expectFile.readLines(Codec.UTF8))
        val obtained = fixWindowsPaths(targetFile.readLines(Codec.UTF8))
        if expected != obtained then
          val instructions = FileDiff.diffMessage(expectFile.path, targetFile.path)
          fail(s"Coverage report differs from expected data.\n$instructions")

      expectMeasurementFile match
        case Some(expectMeasurementFile) if run =>
          // Note that this assumes that the test invoked was single threaded,
          // if that is not the case then this will have to be adjusted
          val targetMeasurementFile = findMeasurementFile(targetDir)
          if updateCheckFiles then
            targetMeasurementFile.copyTo(expectMeasurementFile)
          else
            val targetMeasurementFile = findMeasurementFile(targetDir)
            val expectedMeasurements = fixWindowsPaths(expectMeasurementFile.readLines(Codec.UTF8))
            val obtainedMeasurements = fixWindowsPaths(targetMeasurementFile.readLines(Codec.UTF8))
            if expectedMeasurements != obtainedMeasurements then
              val instructions = FileDiff.diffMessage(expectMeasurementFile.path, targetMeasurementFile.path)
              fail(s"Measurement report differs from expected data.\n$instructions")
        case _ => ()
      ()
    })

  /** Generates the coverage report for the given input file, in a temporary directory. */
  def computeCoverageInTmp(inputFile: FileSystemEntry, isDirectory: Boolean, sourceRoot: FileContainer, run: Boolean)(using TestGroup): FileContainer =
    val target = FileContainer.createTemporaryOnDisk("coverage")
    val options = defaultOptions.and("-Ycheck:instrumentCoverage", "-coverage-out", target.path, "-sourceroot", sourceRoot.path)
    if run then
      val path = if isDirectory then inputFile.path else inputFile.parent.path
      val test = compileDir(path, options)
      test.checkRuns()
    else
      val test =
        if isDirectory then compileDir(inputFile.path, options)
        else compileFile(inputFile.path, options)
      test.checkCompile()
    target

  def checkCoverageWarningsIn(dir: FileContainer)(using TestGroup): Unit =
    def runOnFile(p: File): Boolean =
      p.extension == FileExtension.Scala
      && (Properties.testsFilter.isEmpty || Properties.testsFilter.exists(p.path.contains))

    dir.entries.collect { case f: File if runOnFile(f) => f }.foreach { path =>
      val target = FileContainer.createTemporaryOnDisk("coverage-warning")
      val options = defaultOptions.and("-Ycheck:instrumentCoverage", "-coverage-out", target.path, "-sourceroot", rootSrc.path)
      //val relativePath = Paths.get(userDir).relativize(path).toString
      compileFile(path.path, options).checkWarnings()
    }

  private def findMeasurementFile(targetDir: FileContainer): File = {
    val allFilesInTarget = targetDir.entries.collect { case f: File => f }.toList
    allFilesInTarget.find(_.name.startsWith("scoverage.measurements.")).getOrElse(
      throw new AssertionError(s"Expected to find measurement file in targetDir [${targetDir.path}] but none were found.")
    )
  }

  @Test
  def checkIncrementalCoverage(): Unit =
    val target = FileContainer.createTemporaryOnDisk("coverage")
    val sourceRoot = target.getOrCreateContainer("src")
    val sourceFile1 = sourceRoot.getOrCreateFile("file1", FileExtension.Scala)
    sourceFile1.writeText("def file1() = 1", Codec.UTF8)

    val coverageOut = target.getOrCreateContainer("coverage-out")
    val options = defaultOptions.and("-Ycheck:instrumentCoverage", "-coverage-out", coverageOut.path, "-sourceroot", sourceRoot.path)
    compileFile(sourceFile1.path, options).checkCompile()

    val scoverageFile = coverageOut.getFile("scoverage.coverage")
    assert(scoverageFile.nonEmpty, s"Expected scoverage file to exist at ${coverageOut.path}")

    locally {
      val coverage = Serializer.deserialize(java.nio.file.Path.of(scoverageFile.get.path), sourceRoot.path)
      val filesWithCoverage = coverage.statements.map(_.location.sourcePath.getFileName.toString).toSet
      assertEquals(Set("file1.scala"), filesWithCoverage)
    }

    val sourceFile2 = sourceRoot.getOrCreateFile("file2", FileExtension.Scala)
    sourceFile2.writeText("def file2() = 2", Codec.UTF8)

    compileFile(sourceFile2.path, options).checkCompile()
    locally {
      val coverage = Serializer.deserialize(java.nio.file.Path.of(scoverageFile.get.path), sourceRoot.path)
      val filesWithCoverage = coverage.statements.map(_.location.sourcePath.getFileName.toString).toSet
      assertEquals(Set("file1.scala", "file2.scala"), filesWithCoverage)
    }
    target.deleteRecursively()

  @Test
  def `deleted source files should not be kept in incremental coverage`(): Unit =
    val target = FileContainer.createTemporaryOnDisk("coverage")
    val sourceRoot = target.getOrCreateContainer("src")
    val sourceFile1 = sourceRoot.getOrCreateFile("file1", FileExtension.Scala)
    sourceFile1.writeText("def file1() = 1", Codec.UTF8)

    val coverageOut = target.getOrCreateContainer("coverage-out")
    val options = defaultOptions.and("-Ycheck:instrumentCoverage", "-coverage-out", coverageOut.path, "-sourceroot", sourceRoot.path)
    compileFile(sourceFile1.path, options).checkCompile()

    val scoverageFile = coverageOut.getFile("scoverage.coverage")
    assert(scoverageFile.nonEmpty, s"Expected scoverage file to exist at ${coverageOut.path}")

    locally {
      val coverage = Serializer.deserialize(java.nio.file.Path.of(scoverageFile.get.path), sourceRoot.path)
      val filesWithCoverage = coverage.statements.map(_.location.sourcePath.getFileName.toString).toSet
      assertEquals(Set("file1.scala"), filesWithCoverage)
    }

    val sourceFile2 = sourceRoot.getOrCreateFile("file2", FileExtension.Scala)
    sourceFile2.writeText("def file2() = 2", Codec.UTF8)

    sourceFile1.delete()

    compileFile(sourceFile2.path, options).checkCompile()
    locally {
      val coverage = Serializer.deserialize(java.nio.file.Path.of(scoverageFile.get.path), sourceRoot.path)
      val filesWithCoverage = coverage.statements.map(_.location.sourcePath.getFileName.toString).toSet
      assertEquals(Set("file2.scala"), filesWithCoverage)
    }
    target.deleteRecursively()

object CoverageTests extends ParallelTesting:
  import scala.concurrent.duration.*

  def maxDuration = 30.seconds
  def numberOfWorkers = 1

  def safeMode = Properties.testsSafeMode
  def testFilter = Properties.testsFilter
  def isInteractive = SummaryReport.isInteractive
  def updateCheckFiles = Properties.testsUpdateCheckfile
  def failedTests = TestReporter.lastRunFailedTests

  given summaryReport: SummaryReporting = SummaryReport()
  @AfterClass def tearDown(): Unit =
    super.cleanup()
    summaryReport.echoSummary()

  given TestGroup = TestGroup("instrumentCoverage")
