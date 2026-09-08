package dotty
package tools
package dotc

import java.io.{File => JFile}

import org.junit.{AfterClass, Test}
import org.junit.experimental.categories.Category

import scala.concurrent.duration.*
import reporting.TestReporter
import vulpix.*


class IdempotencyTests {
  import TestConfiguration.*
  import IdempotencyTests.{*, given}
  import CompilationTest.aggregateTests

  // ignore flaky tests
  val filter = FileFilter.NoFilter

  @Category(Array(classOf[SlowTests]))
  @Test def idempotency: Unit = {
    val opt = defaultOptions

    val posIdempotency = {
      given TestGroup("idempotency pos")
      aggregateTests(
        compileFilesInDir("tests/pos", opt, filter),
        compileFilesInDir("tests/pos", opt, filter),
      )
    }

    val orderIdempotency = {
      given TestGroup("idempotency order")
      val tests =
        for {
          testDir <- new JFile("tests/order-idempotency").listFiles() if testDir.isDirectory
        } yield {
          val sources = TestSources.sources(testDir.toPath)
          aggregateTests(
            compileList(testDir.getName, sources, opt),
            compileList(testDir.getName, sources.reverse, opt)
          )
        }
      aggregateTests(tests*)
    }

    def check(name: String) = {
      given TestGroup("idempotency check")
      val files = List(s"tests/idempotency/$name.scala", "tests/idempotency/IdempotencyCheck.scala")
      compileList(name, files, defaultOptions)
    }
    val allChecks = aggregateTests(
      check("CheckOrderIdempotency"),
      // Disabled until strawman is fixed
      // check("CheckStrawmanIdempotency"),
      check("CheckPosIdempotency")
    )

    val allTests = aggregateTests(orderIdempotency, posIdempotency)

    val tests = allTests.keepOutput.checkCompile()
    allChecks.checkRuns()
    tests.delete()
  }

}

object IdempotencyTests extends ParallelTesting