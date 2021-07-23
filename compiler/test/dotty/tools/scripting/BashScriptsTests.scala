package dotty
package tools
package scripting

import java.io.File
import java.nio.file.{Path, Paths, Files}
import scala.sys.process._

import org.junit.Test

import vulpix.TestConfiguration


/** Verifies correct handling of command line arguments by `dist/bin/scala` and `dist/bin/scalac`.
 *   +. arguments following a script path must be treated as script arguments
 *   +. preserve script command line arguments.
 */
class BashScriptsTests:
  // classpath tests managed by scripting.ClasspathTests.scala
  def testFiles = scripts("/scripting")

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
  lazy val (bashExe, bashPath) =
    val bexe = getBashPath
    val bpath = Paths.get(bexe)
    // printf("bashExe: [%s]\n", bexe)
    (bexe, bpath)

  val showArgsScript = testFiles.find(_.getName == "showArgs.sc").get.absPath

  val scalacPath = "dist/target/pack/bin/scalac" // which("scalac")
  val scalaPath = "dist/target/pack/bin/scala"   // which("scala")

  /* verify `dist/bin/scalac` */
  @Test def verifyScalacArgs =
    printf("scalacPath[%s]\n", scalacPath)
    val commandline = (Seq(scalacPath, "-script", showArgsScript) ++ testScriptArgs).mkString(" ")
    if bashPath.toFile.exists then
      var cmd = Array(bashExe, "-c", commandline)
      val output = Process(cmd).lazyLines_!
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
      var mismatches = List.empty[(String, String)]
      for (line, expect) <- output zip expectedOutput do
        printf("expected: %-17s\nactual  : %s\n", expect, line)
        if line != expect then
          fail = true

      if fail then
        assert(output == expectedOutput)

  /*
   * verify that scriptPath.sc sees a valid script.path property.
   */
  @Test def verifyScriptPathProperty =
    val scriptFile = testFiles.find(_.getName == "scriptPath.sc").get
    val expected = s"/${scriptFile.getName}"
    printf("===> verify valid system property script.path is reported by script [%s]\n", scriptFile.getName)
    val (exitCode, stdout, stderr) = bashCommand(scriptFile.absPath)
    if exitCode == 0 && ! stderr.exists(_.contains("Permission denied")) then
      // var cmd = Array(bashExe, "-c", scriptFile.absPath)
      // val stdout = Process(cmd).lazyLines_!
      stdout.foreach { printf("######### [%s]\n", _) }
      val valid = stdout.exists { _.endsWith(expected) }
      if valid then printf("# valid script.path reported by [%s]\n", scriptFile.getName)
      assert(valid, s"script ${scriptFile.absPath} did not report valid script.path value")

  /*
   * verify SCALA_OPTS can specify an @argsfile when launching a scala script in `dist/bin/scala`.
   */
  @Test def verifyScalaOpts =
    val scriptFile = testFiles.find(_.getName == "classpathReport.sc").get
    printf("===> verify valid system property script.path is reported by script [%s]\n", scriptFile.getName)
    val argsfile = createArgsFile() // avoid problems caused by drive letter
    val envPairs = List(("SCALA_OPTS", s"@$argsfile"))
    val (exitCode, stdout, stderr) = bashCommand(scriptFile.absPath, envPairs:_*)
    if exitCode != 0 || stderr.exists(_.contains("Permission denied")) then
      stderr.foreach { System.err.printf("stderr [%s]\n", _) }
      printf("unable to execute script, return value is %d\n", exitCode)
    else
      // val stdout: Seq[String] = Process(cmd, cwd, envPairs:_*).lazyLines_!.toList
      val expected = s"${cwd.toString}"
      val List(line1: String, line2: String) = stdout.take(2)
      val valid = line2.dropWhile( _ != ' ').trim.startsWith(expected)
      if valid then printf(s"\n===> success: classpath begins with %s, as reported by [%s]\n", cwd, scriptFile.getName)
      assert(valid, s"script ${scriptFile.absPath} did not report valid java.class.path first entry")

  lazy val cwd = Paths.get(dotty.tools.dotc.config.Properties.userDir).toFile

  def createArgsFile(): String =
    val utfCharset = java.nio.charset.StandardCharsets.UTF_8.name
    val text = s"-classpath ${cwd.absPath}"
    val path = Files.createTempFile("scriptingTest", ".args")
    Files.write(path, text.getBytes(utfCharset))
    path.toFile.getAbsolutePath.replace('\\', '/')

  extension (str: String) def dropExtension: String =
    str.reverse.dropWhile(_ != '.').drop(1).reverse

  extension(f: File) def absPath: String =
    f.getAbsolutePath.replace('\\', '/')

  lazy val osname = Option(sys.props("os.name")).getOrElse("").toLowerCase

  def getBashPath: String =
    var whichBash = ""
    //printf("osname[%s]\n", osname)
    if osname.startsWith("windows") then
      whichBash = which("bash.exe")
    else
      whichBash = which("bash")

    whichBash

  def bashCommand(cmdstr: String, envPairs: (String, String)*): (Int, Seq[String], Seq[String]) = {
    import scala.sys.process._
    val cmd = Seq(bashExe, "-c", cmdstr)
    val proc = Process(cmd, None, envPairs *)
    var (stdout, stderr) = (List.empty[String], List.empty[String])
    val exitVal = proc ! ProcessLogger (
      (out: String) => stdout ::= out, 
      (err: String) => stderr ::= err
    )
    (exitVal, stdout.reverse, stderr.reverse)
  }

  def execCmd(command: String, options: String *): Seq[String] =
    val cmd = (command :: options.toList).toSeq
    for {
      line <- Process(cmd).lazyLines_!
    } yield line
