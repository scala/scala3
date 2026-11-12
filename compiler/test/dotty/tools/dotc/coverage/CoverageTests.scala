package dotty.tools.dotc.coverage

import org.junit.Test
import org.junit.AfterClass
import org.junit.Assert.*
import org.junit.Assume.*
import org.junit.experimental.categories.Category
import dotty.{BootstrappedOnlyTests, Properties}
import dotty.tools.vulpix.*
import dotty.tools.vulpix.TestConfiguration.*
import dotty.tools.dotc.reporting.TestReporter

import java.nio.file.{FileSystems, Files, Path, Paths, StandardCopyOption}
import scala.jdk.CollectionConverters.*
import scala.util.Properties.userDir
import scala.collection.mutable.Buffer

import java.nio.charset.StandardCharsets
import java.util.stream.Collectors

@Category(Array(classOf[BootstrappedOnlyTests]))
class CoverageTests:
  import CoverageTests.{*, given}
  given TestGroup = TestGroup("instrumentCoverage")

  private val scalaFile = FileSystems.getDefault.getPathMatcher("glob:**.scala")
  private val rootSrc = Paths.get(userDir, "tests", "coverage")

  @Test
  def checkCoverageStatements(): Unit =
    checkCoverageIn(rootSrc.resolve("pos"), false)

  @Test
  def checkInstrumentedRuns(): Unit =
    checkCoverageIn(rootSrc.resolve("run"), true)

  @Test
  def checkCoverageWarnings(): Unit =
    checkCoverageWarningsIn(rootSrc.resolve("warn"))

  def checkCoverageIn(dir: Path, run: Boolean)(using TestGroup): Unit =
    /** Converts \\ (escaped \) to / on windows, to make the tests pass without changing the serialization. */
    def fixWindowsPaths(lines: Buffer[String]): Buffer[String] =
      val separator = java.io.File.separatorChar
      if separator == '\\' then
        val escapedSep = "\\\\"
        lines.map(_.replace(escapedSep, "/"))
      else
        lines
    end fixWindowsPaths

    def runOnFileOrDir(p: Path): Boolean =
      (scalaFile.matches(p) || Files.isDirectory(p))
      && (p != dir)
      && (Properties.testsFilter.isEmpty || Properties.testsFilter.exists(p.toString.contains))

    Files.walk(dir, 1).filter(runOnFileOrDir).forEach(path => {
      // measurement files only exist in the "run" category
      // as these are generated at runtime by the scala.runtime.coverage.Invoker
      val (targetDir, expectFile, expectMeasurementFile) =
        if Files.isDirectory(path) then
          val dirName = path.getFileName.toString
          assert(!Files.walk(path).filter(scalaFile.matches(_)).toArray.isEmpty, s"No scala files found in test directory: ${path}")
          val targetDir = computeCoverageInTmp(path, isDirectory = true, dir, run)
          (targetDir, path.resolve(s"test.scoverage.check"), path.resolve(s"test.measurement.check"))
        else
          val fileName = path.getFileName.toString.stripSuffix(".scala")
          val targetDir = computeCoverageInTmp(path, isDirectory = false, dir, run)
          (targetDir, path.resolveSibling(s"${fileName}.scoverage.check"), path.resolveSibling(s"${fileName}.measurement.check"))

      val targetFile = targetDir.resolve(s"scoverage.coverage")

      if Properties.testsUpdateCheckfile then
        Files.copy(targetFile, expectFile, StandardCopyOption.REPLACE_EXISTING)
      else
        val expected = fixWindowsPaths(Files.readAllLines(expectFile).asScala)
        val obtained = fixWindowsPaths(Files.readAllLines(targetFile).asScala)
        if expected != obtained then
          val instructions = FileDiff.diffMessage(expectFile.toString, targetFile.toString)
          fail(s"Coverage report differs from expected data.\n$instructions")

      if run && Files.exists(expectMeasurementFile) then

        // Note that this assumes that the test invoked was single threaded,
        // if that is not the case then this will have to be adjusted
        val targetMeasurementFile = findMeasurementFile(targetDir)

        if Properties.testsUpdateCheckfile then
          Files.copy(targetMeasurementFile, expectMeasurementFile, StandardCopyOption.REPLACE_EXISTING)

        else
          val targetMeasurementFile = findMeasurementFile(targetDir)
          val expectedMeasurements = fixWindowsPaths(Files.readAllLines(expectMeasurementFile).asScala)
          val obtainedMeasurements = fixWindowsPaths(Files.readAllLines(targetMeasurementFile).asScala)
          if expectedMeasurements != obtainedMeasurements then
            val instructions = FileDiff.diffMessage(expectMeasurementFile.toString, targetMeasurementFile.toString)
            fail(s"Measurement report differs from expected data.\n$instructions")
      ()
    })

  /** Generates the coverage report for the given input file, in a temporary directory. */
  def computeCoverageInTmp(inputFile: Path, isDirectory: Boolean, sourceRoot: Path, run: Boolean)(using TestGroup): Path =
    val target = Files.createTempDirectory("coverage")
    val options = defaultOptions.and("-Ycheck:instrumentCoverage", "-coverage-out", target.toString, "-sourceroot", sourceRoot.toString)
    if run then
      val path = if isDirectory then inputFile.toString else inputFile.getParent.toString
      val test = compileDir(path, options)
      // a checkFile exists by construction; perhaps this intends to assert that target.checkFile.isDefined
      for target <- test.targets; checkFile <- target.checkFile do
        assert(checkFile.exists, s"Expected checkfile for $path $checkFile does not exist.")
      test.checkRuns()
    else
      val test =
        if isDirectory then compileDir(inputFile.toString, options)
        else compileFile(inputFile.toString, options)
      test.checkCompile()
    target

  def checkCoverageWarningsIn(dir: Path)(using TestGroup): Unit =
    def runOnFile(p: Path): Boolean =
      scalaFile.matches(p)
      && (Properties.testsFilter.isEmpty || Properties.testsFilter.exists(p.toString.contains))

    Files.walk(dir, 1).filter(runOnFile).forEach { path =>
      val target = Files.createTempDirectory("coverage-warning")
      val options = defaultOptions.and("-Ycheck:instrumentCoverage", "-Ylog:instrumentCoverage", "-coverage-out", target.toString, "-sourceroot", rootSrc.toString)
      val relativePath = Paths.get(userDir).relativize(path).toString
      compileFile(relativePath, options).checkWarnings()
    }

  private def findMeasurementFile(targetDir: Path): Path = {
    val allFilesInTarget = Files.list(targetDir).collect(Collectors.toList).asScala
    allFilesInTarget.find(_.getFileName.toString.startsWith("scoverage.measurements.")).getOrElse(
      throw new AssertionError(s"Expected to find measurement file in targetDir [${targetDir}] but none were found.")
    )
  }

  @Test
  def checkConstructorOnly(): Unit =
    val target = Files.createTempDirectory("coverage-constructor-only")
    val sourceFile = target.resolve("Test.scala")
    val source = """class Repro(using @scala.annotation.constructorOnly ctx: String):
                   |  val values = List(1).map(i => i -> ctx).toMap
                   |  val unrelated = List(2).map(_ + 1)
                   |  def branch(b: Boolean) = if b then 1 else 2
                   |
                   |class Nested(@scala.annotation.constructorOnly ctx: String):
                   |  def keep[A](value: A): A = value
                   |  val values = keep(List(1).map(i => i -> ctx).toMap)
                   |  List(1).map(i => i -> ctx).toMap
                   |
                   |class Parent(val values: Map[Int, String])
                   |class Child(@scala.annotation.constructorOnly ctx: String)
                   |  extends Parent(List(1).map(i => i -> ctx).toMap)
                   |
                   |object Test:
                   |  def main(args: Array[String]): Unit =
                   |    val repro = new Repro(using "value")
                   |    assert(repro.values == Map(1 -> "value"))
                   |    assert(repro.unrelated == List(3))
                   |    assert(repro.branch(true) == 1 && repro.branch(false) == 2)
                   |    assert(new Nested("nested").values == Map(1 -> "nested"))
                   |    assert(new Child("child").values == Map(1 -> "child"))
                   |    for cls <- List(classOf[Repro], classOf[Nested], classOf[Child]) do
                   |      assert(!cls.getDeclaredFields.exists(_.getName == "ctx"))
                   |""".stripMargin
    Files.writeString(sourceFile, source)
    compileFile(sourceFile.toString, defaultOptions).checkRuns()

    val coverageOut = target.resolve("coverage-out")
    val options = defaultOptions.and("-Ycheck:instrumentCoverage", "-coverage-out", coverageOut.toString, "-sourceroot", target.toString)
    compileFile(sourceFile.toString, options).checkRuns()
    val statements = Serializer.deserialize(coverageOut.resolve("scoverage.coverage")).statements
    val excludedLines = Set(2, 8, 9, 13)
    assertFalse("constructor-only expressions must not be instrumented", statements.exists(s => excludedLines(s.line)))
    assertTrue("unrelated initializers must remain instrumented", statements.exists(_.line == 3))
    assertTrue("unrelated branches must remain instrumented", statements.exists(s => s.line == 4 && s.branch))

  @Test
  def checkIncrementalCoverage(): Unit =
    val target = Files.createTempDirectory("coverage")
    val sourceRoot = target.resolve("src")
    Files.createDirectory(sourceRoot)
    val sourceFile1 = sourceRoot.resolve("file1.scala")
    Files.write(sourceFile1, "def file1() = 1".getBytes(StandardCharsets.UTF_8))

    val coverageOut = target.resolve("coverage-out")
    Files.createDirectory(coverageOut)
    val options = defaultOptions.and("-Ycheck:instrumentCoverage", "-coverage-out", coverageOut.toString, "-sourceroot", sourceRoot.toString)
    compileFile(sourceFile1.toString, options).checkCompile()

    val scoverageFile = coverageOut.resolve("scoverage.coverage")
    assert(Files.exists(scoverageFile), s"Expected scoverage file to exist at $scoverageFile")

    locally {
      val coverage = Serializer.deserialize(scoverageFile)
      val filesWithCoverage = coverage.statements.map(s => Path.of(s.location.sourcePath).getFileName.toString).toSet
      assertEquals(Set("file1.scala"), filesWithCoverage)
    }

    val sourceFile2 = sourceRoot.resolve("file2.scala")
    Files.write(sourceFile2, "def file2() = 2".getBytes(StandardCharsets.UTF_8))

    compileFile(sourceFile2.toString, options).checkCompile()
    locally {
      val coverage = Serializer.deserialize(scoverageFile)
      val filesWithCoverage = coverage.statements.map(s => Path.of(s.location.sourcePath).getFileName.toString).toSet
      assertEquals(Set("file1.scala", "file2.scala"), filesWithCoverage)
    }

  @Test
  def `deleted source files should not be kept in incremental coverage`(): Unit =
    val target = Files.createTempDirectory("coverage")
    val sourceRoot = target.resolve("src")
    Files.createDirectory(sourceRoot)
    val sourceFile1 = sourceRoot.resolve("file1.scala")
    Files.write(sourceFile1, "def file1() = 1".getBytes(StandardCharsets.UTF_8))

    val coverageOut = target.resolve("coverage-out")
    Files.createDirectory(coverageOut)
    val options = defaultOptions.and("-Ycheck:instrumentCoverage", "-coverage-out", coverageOut.toString, "-sourceroot", sourceRoot.toString)
    compileFile(sourceFile1.toString, options).checkCompile()

    val scoverageFile = coverageOut.resolve("scoverage.coverage")
    assert(Files.exists(scoverageFile), s"Expected scoverage file to exist at $scoverageFile")

    locally {
      val coverage = Serializer.deserialize(scoverageFile)
      val filesWithCoverage = coverage.statements.map(s => Path.of(s.location.sourcePath).getFileName.toString).toSet
      assertEquals(Set("file1.scala"), filesWithCoverage)
    }

    val sourceFile2 = sourceRoot.resolve("file2.scala")
    Files.write(sourceFile2, "def file2() = 2".getBytes(StandardCharsets.UTF_8))

    Files.delete(sourceFile1)

    compileFile(sourceFile2.toString, options).checkCompile()
    locally {
      val coverage = Serializer.deserialize(scoverageFile)
      val filesWithCoverage = coverage.statements.map(s => Path.of(s.location.sourcePath).getFileName.toString).toSet
      assertEquals(Set("file2.scala"), filesWithCoverage)
    }

object CoverageTests extends ParallelTesting
