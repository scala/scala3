package dotty
package tools
package dotc

import org.junit.{AfterClass, Test}
import vulpix._

import scala.concurrent.duration._

class MissingCoreLibTests extends ParallelTesting {
  import MissingCoreLibTests._
  import TestConfiguration._

  // Test suite configuration --------------------------------------------------

  def maxDuration = 30.seconds
  def numberOfSlaves = 5
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter

  @Test def missingDottyLib: Unit = {
    val classPath = mkClassPath(Jars.dottyCompiler :: Jars.dottyInterfaces :: Jars.dottyExtras) // missing Jars.dottyLib
    val options = noCheckOptions ++ checkOptions ++ yCheckOptions
    compileFile("../tests/neg/nolib/Foo.scala", TestFlags(classPath, options)).checkExpectedErrors()
  }

}

object MissingCoreLibTests {
  implicit val summaryReport: SummaryReporting = new SummaryReport
  @AfterClass def cleanup(): Unit = summaryReport.echoSummary()
}
