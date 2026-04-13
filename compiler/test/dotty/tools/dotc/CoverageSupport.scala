package dotty
package tools
package dotc

import scala.language.unsafeNulls

import java.nio.file.{Files, Paths}
import scala.util.Try
import scala.util.control.NonFatal
import dotty.tools.dotc.coverage.Serializer
import vulpix._
import reporting.TestReporter
import TestSources.scoverageIgnoreExcludelisted

trait CoverageSupport:
  this: ParallelTesting =>
  import ParallelTesting._

  trait CoverageVerification extends Test:
    override def onSuccess(testSource: TestSource, reporters: Seq[TestReporter], logger: LoggedRunnable): Unit =
      super.onSuccess(testSource, reporters, logger)
      verifyCoverageFile(testSource)

  trait CoverageTestSupport[A <: Test]:
    def build(using SummaryReporting): (List[TestSource], Int, Option[Int], Boolean) => Test
    def fallback(test: CompilationTest)(using SummaryReporting): Unit


  /** Custom PosTest that verifies coverage files in onSuccess callback */
  final class PosTestWithCoverage(
    testSources: List[TestSource],
    times: Int,
    threadLimit: Option[Int],
    suppressAllOutput: Boolean
  )(implicit summaryReport: SummaryReporting)
  extends Test(testSources, times, threadLimit, suppressAllOutput) with CoverageVerification

  final class RewriteTestWithCoverage(
    testSources: List[TestSource],
    checkFiles: Map[java.io.File, java.io.File],
    times: Int,
    threadLimit: Option[Int],
    suppressAllOutput: Boolean
  )(implicit summaryReport: SummaryReporting)
  extends RewriteTest(testSources, checkFiles, times, threadLimit, suppressAllOutput) with CoverageVerification

  final class WarnTestWithCoverage(
    testSources: List[TestSource],
    times: Int,
    threadLimit: Option[Int],
    suppressAllOutput: Boolean
  )(implicit summaryReport: SummaryReporting)
  extends WarnTest(testSources, times, threadLimit, suppressAllOutput) with CoverageVerification

  /** Custom RunTest that verifies coverage files in onSuccess callback */
  final class RunTestWithCoverage(
    testSources: List[TestSource],
    times: Int,
    threadLimit: Option[Int],
    suppressAllOutput: Boolean
  )(implicit summaryReport: SummaryReporting)
  extends RunTest(testSources, times, threadLimit, suppressAllOutput) with CoverageVerification


  given CoverageTestSupport[PosTestWithCoverage] with
    def build(using SummaryReporting) = (t, ti, tl, s) => new PosTestWithCoverage(t, ti, tl, s)
    def fallback(test: CompilationTest)(using SummaryReporting): Unit = test.checkCompile()

  given CoverageTestSupport[WarnTestWithCoverage] with
    def build(using SummaryReporting) = (t, ti, tl, s) => new WarnTestWithCoverage(t, ti, tl, s)
    def fallback(test: CompilationTest)(using SummaryReporting): Unit = test.checkWarnings()

  given CoverageTestSupport[RunTestWithCoverage] with
    def build(using SummaryReporting) = (t, ti, tl, s) => new RunTestWithCoverage(t, ti, tl, s)
    def fallback(test: CompilationTest)(using SummaryReporting): Unit = test.checkRuns()


  /** Verifies coverage file exists and is valid for a test source */
  def verifyCoverageFile(testSource: TestSource): Unit =
    val flags = testSource.flags.options
    val idx = flags.indexOf("-coverage-out")
    if (idx >= 0 && idx + 1 < flags.length)
      val coverageDir = Paths.get(flags(idx + 1))
      val coverageFile = coverageDir.resolve("scoverage.coverage")

      try
        assert(Files.exists(coverageFile), s"Coverage file missing: $coverageFile for test ${testSource.title}")
        assert(Files.size(coverageFile) > 0, s"Coverage file is empty: $coverageFile for test ${testSource.title}")

        // Verify file can be deserialized (valid format)
        val sourceRoot = Paths.get(".").toAbsolutePath.toString
        assert(Try(Serializer.deserialize(coverageFile, sourceRoot)).isSuccess, s"Coverage file has invalid format: $coverageFile for test ${testSource.title}")
      finally
        // Cleanup temporary directory even if exceptions are thrown
        try
          Files.walk(coverageDir)
            .sorted(java.util.Comparator.reverseOrder())
            .forEach(Files.delete)
        catch
          case NonFatal(_) => // Ignore cleanup errors
      end try
    end if
  end verifyCoverageFile

  def runWithCoverageOrFallback[A <: Test](test: CompilationTest, desc: String)(using CoverageTestSupport[A], SummaryReporting): Unit =
    val tc = summon[CoverageTestSupport[A]]
    if Properties.testsInstrumentCoverage then
      test.checkPass(tc.build(test.targets, test.times, test.threadLimit, test.shouldFail || test.shouldSuppressOutput), desc)
    else
      tc.fallback(test)

  /** Wraps a CompilationTest to add coverage flags to all targets.
   *  Each target gets its own unique temporary coverage directory.
   *  Filters out test sources that match the scoverage ignore excludelist.
   */
  def withCoverage(test: CompilationTest): CompilationTest = {
    if (Properties.testsInstrumentCoverage) {
      val ignoreList = scoverageIgnoreExcludelisted.toSet

      // Filter out test sources whose filenames or directory names match the excludelist
      val filteredTargets = test.targets.filter { target =>
        val sourceFiles = target.sourceFiles

        def matchesIgnoreList(file: java.io.File): Boolean = {
          // Match by:
          // - file name (e.g. i10848a.scala)
          // - parent directory name (e.g. i18589 for tests/pos-special/i18589/test_1.scala)
          // This makes `compileDir(".../iNNNNN")` skippable even when the directory contains
          // a single `test_1.scala` file (in that case, the target title becomes the file path).
          val nameMatch = ignoreList.contains(file.getName)
          val parentMatch = Option(file.getParentFile).exists(p => ignoreList.contains(p.getName))
          nameMatch || parentMatch
        }

        val fileMatches = sourceFiles.exists(matchesIgnoreList)

        // Also check the basename of the test title (covers SeparateCompilationSource directory targets)
        val titleMatches = ignoreList.contains(new java.io.File(target.title).getName)

        val shouldInclude = !fileMatches && !titleMatches
        if (!shouldInclude) {
          // Log skipped test for visibility
          val testName = if (sourceFiles.length == 1) sourceFiles.head.getName else target.title
          println(s"[Scoverage] Skipping test: $testName (matches scoverage ignore excludelist)")
        }
        shouldInclude
      }

      val modifiedTargets = filteredTargets.map { target =>
        val coverageDir = Files.createTempDirectory("coverage")
        val sourceRoot = Paths.get(".").toAbsolutePath.toString
        target.withFlags(
          "-Ycheck:instrumentCoverage",
          "-coverage-out", coverageDir.toString,
          "-sourceroot", sourceRoot
        )
      }
      test.copy(targets = modifiedTargets)
    } else test
  }
