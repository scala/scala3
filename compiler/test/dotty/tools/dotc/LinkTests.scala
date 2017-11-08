package dotty
package tools
package dotc

import java.io.{File => JFile}
import java.nio.file.{Files, Path, Paths}

import org.junit.{AfterClass, Test}
import org.junit.Assert._
import vulpix._

import scala.concurrent.duration._
import scala.collection.JavaConverters._

class LinkTests extends ParallelTesting {
  import ParallelTesting._
  import TestConfiguration._
  import LinkTests._

  // Test suite configuration --------------------------------------------------

  def maxDuration = 30.seconds
  def numberOfSlaves = 5
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter


  @Test def linkTest: Unit = {
    // Setup and compile libraries
    val strawmanLibGroup = TestGroup("linkTest/strawmanLibrary")
    val strawmanLibTestGroup = TestGroup(strawmanLibGroup + "/tests")

    val linkCustomLibGroup = TestGroup("linkTest/linkCustomLib")
    val linkCustomLibTestGroup = TestGroup(linkCustomLibGroup + "/tests")

    val strawmanLibrary = compileDir("../collection-strawman/collections/src/main", defaultOptions)(strawmanLibGroup)
    val linkCustomLib = compileDir("../tests/link/custom-lib", defaultOptions)(linkCustomLibGroup)

    val libraries = {
      strawmanLibrary +
      linkCustomLib
    }.keepOutput.checkCompile()

    // Setup class paths
    def mkLinkClassFlags(libPath: String) =
      TestFlags(mkClassPath(libPath :: Jars.dottyTestDeps), mkClassPath(Jars.dottyTestDeps), basicDefaultOptions :+ "-Xlink-optimise")
    val strawmanClassPath = mkLinkClassFlags(defaultOutputDir + strawmanLibGroup + "/main/")
    val customLibClassFlags = mkLinkClassFlags(defaultOutputDir + linkCustomLibGroup + "/custom-lib")

    // Link tests
    val linkDir = "../tests/link"
    val linkStramanDir = linkDir + "/strawman"
    val linkCustomLibDir = linkDir + "/on-custom-lib"

    val linkStrawmanTest = compileFilesInDir(linkStramanDir, strawmanClassPath)(strawmanLibTestGroup)
    val linkCustomLibTest = compileFilesInDir(linkCustomLibDir, customLibClassFlags)(linkCustomLibTestGroup)

    def classFileChecks(sourceDir: String, testName: String) = {
      val checkExt = ".classcheck"
      for (check <- new JFile(sourceDir).listFiles().filter(_.toString.endsWith(checkExt))) {
        val outDir = {
          def path(str: String) = str.substring(linkDir.length, str.length - checkExt.length)
          defaultOutputDir + testName + path(check.toString) + "/"
        }
        val expectedClasses = scala.io.Source.fromFile(check).getLines().toSet
        val actualClasses = Files.walk(Paths.get(outDir)).iterator().asScala.collect {
          case f if f.toString.endsWith(".class") => f.toString.substring(outDir.length, f.toString.length - ".class".length)
        }.toSet
        assertEquals(check.toString, expectedClasses, actualClasses)
      }
    }

    // Run all tests
    val tests = {
      linkStrawmanTest +
      linkCustomLibTest
    }.keepOutput.checkRuns()

    try {
      classFileChecks(linkStramanDir, strawmanLibTestGroup.name)
      classFileChecks(linkCustomLibDir, linkCustomLibTestGroup.name)
    } finally {
      (libraries + tests).delete()
    }
  }

}

object LinkTests {
  implicit val summaryReport: SummaryReporting = new SummaryReport
  @AfterClass def cleanup(): Unit = summaryReport.echoSummary()
}
