package dotty.tools
package vulpix

import org.junit.Test
import org.junit.experimental.categories.Category
import scala.concurrent.duration._
import dotty.Properties
import TestConfiguration._

/** Meta tests for the Vulpix test suite. This test follows the structure of
 *  CompilationTests.scala. It is ment to be called from bash to diff with
 *  output againts an expected result.
 */
@Category(Array(classOf[dotty.VulpixMetaTests]))
class VulpixMetaTests extends ParallelTesting {
  def maxDuration = 1.seconds
  def numberOfSlaves = 5
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter
  implicit val summaryReport: SummaryReporting = new SummaryReport
  implicit def testGroup: TestGroup = TestGroup("VulpixMetaTests")

  @Test def compilePos: Unit = compileFilesInDir("../tests/meta-tests/pos", defaultOptions).checkCompile()
  @Test def compileNeg: Unit = compileFilesInDir("../tests/meta-tests/neg", defaultOptions).checkExpectedErrors()
  @Test def runAll: Unit     = compileFilesInDir("../tests/meta-tests/run", defaultOptions).checkRuns()
}
