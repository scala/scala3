package scala.quoted.staging.repl

import dotty.BootstrappedOnlyTests
import dotty.tools.scripts
import dotty.tools.repl.ReplTest
import dotty.tools.vulpix.TestConfiguration
import org.junit.Test
import org.junit.experimental.categories.Category

/** Runs all tests contained in `staging/test-resources/repl-staging` */
class StagingScriptedReplTests extends ReplTest(ReplTest.withStagingOptions) {

  @Category(Array(classOf[BootstrappedOnlyTests]))
  @Test def replStagingTests = scripts("/repl-staging").foreach(testFile)

}
