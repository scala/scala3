package scala.quoted.staging.repl

import dotty.BootstrappedOnlyTests
import dotty.tools.scripts
import dotty.tools.repl.ReplTest
import org.junit.Test
import org.junit.experimental.categories.Category

/** Runs all tests contained in `staging/test-resources/repl-staging` */
class StagingScriptedReplTests extends ReplTest(StagingScriptedReplTests.replOptions) {

  @Category(Array(classOf[BootstrappedOnlyTests]))
  @Test def replStagingTests = testFiles(scripts("/repl-staging"))

}

object StagingScriptedReplTests:
  private def replOptions: Array[String] =
    val extraClasspath = sys.props.get("dotty.tests.classes.scalaLibrary").filter(_.nonEmpty).toSeq
    ReplTest.createOptions(extraClasspath*) ++ Array("-Xrepl-interrupt-instrumentation", "false")
