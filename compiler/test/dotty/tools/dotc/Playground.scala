package dotty.tools.dotc

import dotty.tools.vulpix.*
import org.junit.Test
import org.junit.Ignore

// For ease of debugging individual tests
@Ignore class Playground:
  import TestConfiguration.*
  import CompilationTests.{*, given}
  import CompilationTest.aggregateTests

  @Test def example: Unit =
    implicit val testGroup: TestGroup = TestGroup("single-test")
    // can add, e.g., .and("-some-option")
    val options = defaultOptions
    // can also use `compileDir` (single test as a dir), `compileFilesInDir` (all tests within a dir)
    val test = compileFile("tests/pos/tuple-filter.scala", options)
    // or `RunTestWithCoverage` for "run" tests with output, or `WarnTestWithCoverage` for "warn" tests with warnings
    type TestKind = PosTestWithCoverage
    val compilationTest = withCoverage(aggregateTests(test))
    runWithCoverageOrFallback[TestKind](compilationTest, testGroup.name)

