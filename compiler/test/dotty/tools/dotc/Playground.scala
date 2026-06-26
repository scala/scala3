package dotty.tools.dotc

import dotty.tools.vulpix.*
import org.junit.Test
import org.junit.Ignore

// For ease of debugging individual tests
@Ignore class Playground:
  import TestConfiguration.*
  import CompilationTests.{*, given}
  import CompilationTest.aggregateTests

  @Test def example(): Unit =
    given testGroup: TestGroup = TestGroup("single-test")
    // can add, e.g., .and("-some-option")
    val options = defaultOptions
    // can also use `compileDir` (single test as a dir), `compileFilesInDir` (all tests within a dir)
    val test = compileFile("tests/pos/tuple-filter.scala", options)
    // or `RunTestWithCoverage` for "run" tests with output, or `WarnTestWithCoverage` for "warn" tests with warnings
    type TestKind = PosTestWithCoverage
    val compilationTest = withCoverage(aggregateTests(test))
    runWithCoverageOrFallback[TestKind](compilationTest, testGroup.name)

  @Test def bestEffortTasty(): Unit =
    given testGroup: TestGroup = TestGroup("single-test")
    // can add, e.g., .and("-some-option")
    val options = TestConfiguration.bestEffortBaselineOptions
    // make sure we only test one thing at a time, despite the framework requiring an entire dir + filters
    val testFile = "26037.scala"
    compileBestEffortTastyInDir(
      "tests/neg", options,
      picklingFilter = FileFilter.include(List(testFile)),
      unpicklingFilter = FileFilter.include(List(testFile))
    ).checkNoCrash()
