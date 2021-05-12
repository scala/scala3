package dotty
package tools
package scripting

import java.io.File
import java.nio.file.{Path, Paths, Files}

import org.junit.Test

import vulpix.TestConfiguration


/** Runs all tests contained in `compiler/test-resources/scripting/` */
class BashScriptsTests:
  // classpath tests managed by scripting.ClasspathTests.scala
  def testFiles = scripts("/scripting").filter { ! _.getName.startsWith("classpath") }

  @Test def verifyScriptArgs =
    import scala.sys.process._
    val showArgsScript = testFiles.find(_.getName == "showArgs.sc").get.absPath
    val scalacPath = which("scalac")
    val commandline = Seq(scalacPath, "-script", showArgsScript, "a", "b", "c", "-repl", "-run", "-script", "-debug").mkString(" ")
    val bashExe = getBashPath
    val bashPath = Paths.get(bashExe)
    printf("bashExe: [%s]\n", bashExe)
    if bashPath.toFile.exists then
      var cmd = Array(bashExe, "-c", commandline)
      val output = for {
        line <- Process(cmd).lazyLines_!
      } yield line
      val expected = Seq(
      "arg  0:[a]", 
      "arg  1:[b]", 
      "arg  2:[c]", 
      "arg  3:[-repl]", 
      "arg  4:[-run]", 
      "arg  5:[-script]", 
      "arg  6:[-debug]", 
      )
      var fail = false
      printf("\n")
      for (line, expect) <- output zip expected do
        printf("expected: %-17s| actual: %s\n", line, expect)
        if line != expect then
          fail = true

      if fail then
        assert(output == expected)

  extension (str: String) def dropExtension =
    str.reverse.dropWhile(_ != '.').drop(1).reverse

  extension(f: File) def absPath =
    f.getAbsolutePath.replace('\\', '/')

  lazy val osname = Option(sys.props("os.name")).getOrElse("").toLowerCase

  def getBashPath: String =
    var whichBash = ""
    printf("osname[%s]\n", osname)
    if osname.startsWith("windows") then
      whichBash = which("bash.exe")
      /*
      val testCygpath = which("cygpath.exe")
      printf("testCygpath[%s]\n", testCygpath)
      if testCygpath.nonEmpty then
        whichBash = execCmd(testCygpath, "-m", whichBash).mkString(" ").trim
        printf("whichBash[%s]\n", whichBash)
        */

    else
      whichBash = which("bash")

    whichBash

  def execCmd(command: String, options: String *): Seq[String] =
    val cmd = (command :: options.toList).toSeq
    import scala.sys.process._
    for {
      line <- Process(cmd).lazyLines_!
    } yield line
