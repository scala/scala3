package dotty
package tools
package dotc

import scala.language.unsafeNulls

import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import scala.util.Try
import scala.util.control.NonFatal
import dotty.tools.dotc.coverage.Serializer
import vulpix._
import reporting.TestReporter
import TestSources.scoverageIgnoreExcludelisted

trait CoverageSupport { this: ParallelTesting =>
  import ParallelTesting._

  /** Verifies coverage file exists and is valid for a test source */
  def verifyCoverageFile(testSource: TestSource): Unit = {
    val flags = testSource.flags.options
    val idx = flags.indexOf("-coverage-out")
    if (idx >= 0 && idx + 1 < flags.length) {
      val coverageDir = Paths.get(flags(idx + 1))
      val coverageFile = coverageDir.resolve("scoverage.coverage")

      try {
        assert(Files.exists(coverageFile), s"Coverage file missing: $coverageFile for test ${testSource.title}")
        assert(Files.size(coverageFile) > 0, s"Coverage file is empty: $coverageFile for test ${testSource.title}")

        // Verify file can be deserialized (valid format)
        val sourceRoot = Paths.get(".").toAbsolutePath.toString
        assert(Try(Serializer.deserialize(coverageFile, sourceRoot)).isSuccess, s"Coverage file has invalid format: $coverageFile for test ${testSource.title}")
      } finally {
        // Cleanup temporary directory even if exceptions are thrown
        try {
          Files.walk(coverageDir)
            .sorted(java.util.Comparator.reverseOrder())
            .forEach(Files.delete)
        } catch {
          case NonFatal(_) => // Ignore cleanup errors
        }
      }
    }
  }

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

  /** Custom PosTest that verifies coverage files in onSuccess callback */
  final class PosTestWithCoverage(
    testSources: List[TestSource],
    times: Int,
    threadLimit: Option[Int],
    suppressAllOutput: Boolean
  )(implicit summaryReport: SummaryReporting)
  extends Test(testSources, times, threadLimit, suppressAllOutput) {
    override def onSuccess(testSource: TestSource, reporters: Seq[TestReporter], logger: LoggedRunnable): Unit = {
      verifyCoverageFile(testSource)
    }
  }

  /** Custom RunTest that verifies coverage files in onSuccess callback */
  final class RunTestWithCoverage(
    testSources: List[TestSource],
    times: Int,
    threadLimit: Option[Int],
    suppressAllOutput: Boolean
  )(implicit summaryReport: SummaryReporting)
  extends RunTest(testSources, times, threadLimit, suppressAllOutput) {
    override def onSuccess(testSource: TestSource, reporters: Seq[TestReporter], logger: LoggedRunnable): Unit = {
      // First, run the standard run test verification (checks output, runs main, etc.)
      super.onSuccess(testSource, reporters, logger)
      // Then verify coverage file
      verifyCoverageFile(testSource)
    }
  }
}
