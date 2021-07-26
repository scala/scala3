package dotty
package tools
package scripting

import java.io.File
import java.nio.file.{Path, Paths, Files}
import scala.sys.process._

import org.junit.Test

import vulpix.TestConfiguration


/** Runs all tests contained in `compiler/test-resources/scripting/` */
class BashScriptsTests:
  // classpath tests managed by scripting.ClasspathTests.scala
  def testFiles = scripts("/scripting").filter { ! _.getName.startsWith("classpath") }

  lazy val expectedOutput = List(
    "arg  0:[a]", 
    "arg  1:[b]", 
    "arg  2:[c]", 
    "arg  3:[-repl]", 
    "arg  4:[-run]", 
    "arg  5:[-script]", 
    "arg  6:[-debug]", 
  )
  lazy val testScriptArgs = Seq(
    "a", "b", "c", "-repl", "-run", "-script", "-debug"
  )
  lazy val (bashExe,bashPath) =
    val bexe = getBashPath
    val bpath = Paths.get(bexe)
    printf("bashExe: [%s]\n", bexe)
    (bexe, bpath)

  val showArgsScript = testFiles.find(_.getName == "showArgs.sc").get.absPath

  val scalacPath = "dist/target/pack/bin/scalac" // which("scalac")
  val scalaPath = "dist/target/pack/bin/scala"   // which("scala")

  /* verify `dist/bin/scalac` */
  @Test def verifyScalacArgs =
    printf("scalacPath[%s]\n",scalacPath)
    val commandline = (Seq(scalacPath, "-script", showArgsScript) ++ testScriptArgs).mkString(" ")
    if bashPath.toFile.exists then
      var cmd = Array(bashExe, "-c", commandline)
      val output = for {
        line <- Process(cmd).lazyLines_!
      } yield line
      var fail = false
      printf("\n")
      for (line, expect) <- output zip expectedOutput do
        printf("expected: %-17s\nactual  : %s\n", expect, line)
        if line != expect then
          fail = true

      if fail then
        assert(output == expectedOutput)

  /* verify `dist/bin/scala` */
  @Test def verifyScalaArgs =
    val commandline = (Seq(scalaPath, showArgsScript) ++ testScriptArgs).mkString(" ")
    if bashPath.toFile.exists then
      var cmd = Array(bashExe, "-c", commandline)
      val output = for {
        line <- Process(cmd).lazyLines_!
      } yield line
      var fail = false
      printf("\n")
      var mismatches = List.empty[(String,String)]
      for (line, expect) <- output zip expectedOutput do
        printf("expected: %-17s\nactual  : %s\n", expect, line)
        if line != expect then
          fail = true

      if fail then
        assert(output == expectedOutput)

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
    else
      whichBash = which("bash")

    whichBash

  def execCmd(command: String, options: String *): Seq[String] =
    val cmd = (command :: options.toList).toSeq
    for {
      line <- Process(cmd).lazyLines_!
    } yield line
