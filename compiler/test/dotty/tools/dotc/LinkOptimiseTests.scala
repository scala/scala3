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

class LinkOptimiseTests extends ParallelTesting {
  import ParallelTesting._
  import TestConfiguration._
  import LinkOptimiseTests._

  // Test suite configuration --------------------------------------------------

  def maxDuration = 30.seconds
  def numberOfSlaves = 5
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter


  @Test def linkOptimise: Unit = {
    // Setup and compile libraries
    def strawmanLibrary =
      compileDir("../collection-strawman/src/main", defaultOptions)
    def linkCustomLib =
      compileDir("../tests/link/custom-lib", defaultOptions)

    val libraries = {
      strawmanLibrary +
      linkCustomLib
    }.keepOutput.checkCompile()

    // Setup class paths
    def mkLinkClassFlags(libPath: String) =
      TestFlags(mkClassPath(libPath :: Jars.dottyTestDeps), mkClassPath(Jars.dottyTestDeps), basicDefaultOptions :+ "-Xlink-optimise")
    val strawmanClassPath = mkLinkClassFlags(defaultOutputDir + "strawmanLibrary/main/")
    val customLibClassFlags = mkLinkClassFlags(defaultOutputDir + "linkCustomLib/custom-lib")

    // Link tests
    val linkDir = "../tests/link"
    val linkStramanDir = linkDir + "/strawman"
    val linkCustomLibDir = linkDir + "/on-custom-lib"
    def linkStrawmanTest = compileFilesInDir(linkStramanDir, strawmanClassPath)
    def linkCustomLibTest = compileFilesInDir(linkCustomLibDir, customLibClassFlags)

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
      classFileChecks(linkStramanDir, "linkStrawmanTest")
      classFileChecks(linkCustomLibDir, "linkCustomLibTest")
    } finally {
      (libraries + tests).delete()
    }
  }

}

object LinkOptimiseTests {
  implicit val summaryReport: SummaryReporting = new SummaryReport
  @AfterClass def cleanup(): Unit = summaryReport.echoSummary()
}
