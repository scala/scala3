package dotc

import java.io.{File => JFile}

import dotty.Jars
import dotty.tools.dotc.CompilerTest
import org.junit.{Before, Test}

import scala.io.Source
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

  implicit val defaultOptions = noCheckOptions ++ {
    if (isRunByJenkins) List("-Ycheck:tailrec,resolveSuper,mixin,restoreScopes,labelDef") // should be Ycheck:all, but #725
    else List("-Ycheck:tailrec,resolveSuper,mixin,restoreScopes,labelDef")
  } ++ checkOptions ++ classPath

  val scala2mode = List("-language:Scala2")

  private val linkDCE = List("-link-dce", "-Ylink-dce-checks", "-Ylog:callGraph")
  private val linkDCEwithVis = "-link-vis" :: linkDCE

  private val testsDir      = "../tests/"
  private val linkDCEWithStdlibDir = testsDir + "link-dce-stdlib/"

  @Before def cleanup(): Unit = {
    // remove class files from stdlib and tests compilation
    Directory(defaultOutputDir + "scala").deleteRecursively()
    Directory(defaultOutputDir + "java").deleteRecursively()
  }

  @Test def compileStdLib(): Unit = compileList("compileStdLib", stdlibFiles, "-migration" :: "-Yno-inline" :: scala2mode)

  // Test callgraph DCE on code that use DCEed stdlib
  @Test def link_dce_stdlib_all(): Unit =
    runFiles(linkDCEWithStdlibDir, scala2mode ::: linkDCE, stdlibFiles = linkDCEStdlibFiles)

  @org.junit.Ignore("Too long to run in CI")
   def link_dce_vis_stdlib_all(): Unit =
    runFiles(linkDCEWithStdlibDir, scala2mode ::: linkDCEwithVis, stdlibFiles = linkDCEStdlibFiles)

  private def loadList(path: String): List[String] = Source.fromFile(path, "UTF8").getLines()
    .map(_.trim) // allow identation
    .filter(!_.startsWith("#")) // allow comment lines prefixed by #
    .map(_.takeWhile(_ != '#').trim) // allow comments in the end of line
    .filter(_.nonEmpty)
    .toList

  private def stdlibWhitelistFile: String = "./test/dotc/scala-collections.whitelist"

  private val stdlibFiles: List[String] = loadList(stdlibWhitelistFile)
  private val dottyStdlibFiles: List[String] = loadList("./test/dotc/dotty-library.whitelist")
  private val linkDCEStdlibFiles: List[String] = dottyStdlibFiles ::: stdlibFiles

}
