package dotc

import java.io.{File => JFile}

import dotty.Jars
import dotty.tools.StdLibSources
import dotty.tools.dotc.CompilerTest
import org.junit.Assert._
import org.junit.{Before, Test}

import scala.reflect.io.Directory

// tests that match regex '(pos|dotc|run|java|compileStdLib)\.*' would be executed as benchmarks.
class stdlibTests extends CompilerTest {

  def isRunByJenkins: Boolean = sys.props.isDefinedAt("dotty.jenkins.build")

  val defaultOutputDir = "../out/"

  val noCheckOptions = List(
//    "-verbose",
//    "-Ylog:frontend",
//    "-Xprompt",
//    "-explaintypes",
//    "-Yshow-suppressed-errors",
    "-pagewidth", "120",
    "-d", defaultOutputDir
  )

  val checkOptions = List(
    "-Yno-deep-subtypes",
    "-Yno-double-bindings",
    "-Yforce-sbt-phases",
    "-color:never"
  )

  val classPath = {
    val paths = Jars.dottyTestDeps map { p =>
      val file = new JFile(p)
      assert(
        file.exists,
        s"""|File "$p" couldn't be found. Run `packageAll` from build tool before
            |testing.
            |
            |If running without sbt, test paths need to be setup environment variables:
            |
            | - DOTTY_LIBRARY
            | - DOTTY_COMPILER
            | - DOTTY_INTERFACES
            | - DOTTY_EXTRAS
            |
            |Where these all contain locations, except extras which is a colon
            |separated list of jars.
            |
            |When compiling with eclipse, you need the sbt-interfaces jar, put
            |it in extras."""
      )
      file.getAbsolutePath
    } mkString (":")

    List("-classpath", paths)
  }

  val basicOptions = noCheckOptions ++ checkOptions ++ classPath

  implicit val defaultOptions = {
    if (isRunByJenkins) List("-Ycheck:tailrec,resolveSuper,mixin,restoreScopes,labelDef") // should be Ycheck:all, but #725
    else List("-Ycheck:tailrec,resolveSuper,mixin,restoreScopes,labelDef")
  } ++ basicOptions

  val stdlibMode = List("-migration", "-Yno-inline", "-language:Scala2")

  private val linkDCE = List("-link-dce", "-Ylink-dce-checks", "-Ylog:callGraph", "-Ylink-stdlib")
  private val linkDCEwithVis = "-link-vis" :: linkDCE

  private val testsDir      = "../tests/"
  private val linkDCEWithStdlibDir = testsDir + "link-dce-stdlib/"

  @Before def cleanup(): Unit = {
    // remove class files from stdlib and tests compilation
    Directory(defaultOutputDir + "scala").deleteRecursively()
    Directory(defaultOutputDir + "java").deleteRecursively()
  }

  @Test def checkWBLists(): Unit = {
    val stdlibFilesBlackListed = StdLibSources.blacklisted

    val duplicates = stdlibFilesBlackListed.groupBy(x => x).filter(_._2.size > 1).filter(_._2.size > 1)
    val msg = duplicates.map(x => s"'${x._1}' appears ${x._2.size} times").mkString(s"Duplicate entries in ${StdLibSources.blacklistFile}:\n", "\n", "\n")
    assertTrue(msg, duplicates.isEmpty)

    val filesNotInStdLib = stdlibFilesBlackListed.toSet -- StdLibSources.all
    val msg2 = filesNotInStdLib.map(x => s"'$x'").mkString(s"Entries in ${StdLibSources.blacklistFile} where not found:\n", "\n", "\n")
    assertTrue(msg2, filesNotInStdLib.isEmpty)
  }

  @Test def compileStdLib(): Unit = compileList("compileStdLib", stdlibFiles, stdlibMode)

  // Test callgraph DCE on code that use DCEed stdlib
  @Test def link_dce_stdlib_all(): Unit =
    runFiles(linkDCEWithStdlibDir, stdlibMode ::: linkDCE, stdlibFiles = linkDCEStdlibFiles)(basicOptions)

  @org.junit.Ignore("Too long to run in CI")
  @Test def link_dce_vis_stdlib_all(): Unit =
    runFiles(linkDCEWithStdlibDir, stdlibMode ::: linkDCEwithVis, stdlibFiles = linkDCEStdlibFiles)(basicOptions)

  private def stdlibWhitelistFile: String = "./test/dotc/scala-collections.whitelist"
  private def stdlibBlackFile: String = "./test/dotc/scala-collections.blacklist"

  private val stdlibFiles: List[String] = StdLibSources.whitelisted
  private val dottyStdlibFiles: List[String] = StdLibSources.loadList("./test/dotc/dotty-library.whitelist", "../library/src/")
  private val linkDCEStdlibFiles: List[String] = dottyStdlibFiles ::: stdlibFiles

}
