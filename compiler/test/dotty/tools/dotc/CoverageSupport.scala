package dotty
package tools
package dotc

import scala.language.unsafeNulls

import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
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
        if (!Files.exists(coverageFile)) {
          throw new AssertionError(s"Coverage file missing: $coverageFile for test ${testSource.title}")
        }

        if (Files.size(coverageFile) == 0) {
          throw new AssertionError(s"Coverage file is empty: $coverageFile for test ${testSource.title}")
        }

        // Verify file can be deserialized (valid format)
        try {
          val sourceRoot = Paths.get(".").toAbsolutePath.toString
          Serializer.deserialize(coverageFile, sourceRoot)
        } catch {
          case e: Exception =>
            throw new AssertionError(s"Coverage file has invalid format: $coverageFile for test ${testSource.title}: ${e.getMessage}")
        }
      } finally {
        // Cleanup temporary directory even if exceptions are thrown
        try {
          Files.walk(coverageDir)
            .sorted(java.util.Comparator.reverseOrder())
            .forEach(Files.delete)
        } catch {
          case _: Exception => // Ignore cleanup errors
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
        // Check individual file names
        val fileMatches = sourceFiles.exists { file =>
          ignoreList.contains(file.getName)
        }
        // Check directory name from test title (for directory tests)
        val titleMatches = {
          val title = target.title
          // Extract the last component (directory name or filename) from the path
          val lastComponent = new java.io.File(title).getName
          ignoreList.contains(lastComponent)
        }
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
