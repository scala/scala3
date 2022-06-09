package dotty
package tools
package dotc

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._
import org.junit.experimental.categories.Category

import java.io.File
import java.nio.file._
import java.util.stream.{ Stream => JStream }
import scala.util.matching.Regex
import scala.concurrent.duration._
import vulpix._

class NamedPatternMatching {
  import ParallelTesting._
  import TestConfiguration._
  import CompilationTests._
  import CompilationTest.aggregateTests

  // Positive tests ------------------------------------------------------------

  @Test def pos: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compilePos")
    aggregateTests(
      compileFile("tests/pos/namedPatternMatching.scala", defaultOptions),
    ).checkCompile()
  }

  @Test def negativeTests: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compileNeg")
    aggregateTests(
      compileFile("tests/neg/negNamedPatternMatching.scala", defaultOptions),
      compileFile("tests/neg/bad-unapplies.scala", defaultOptions),
      compileFile("tests/neg/i10757.scala", defaultOptions),
    ).checkExpectedErrors()
  }

  @Test def executionTest: Unit = {
    implicit val testGroup: TestGroup = TestGroup("runPos")
    aggregateTests(
      compileFile("tests/run/runNamedPatternMatching.scala", defaultOptions),
    ).checkRuns()
  }

}
