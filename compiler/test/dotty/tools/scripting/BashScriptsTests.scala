package dotty
package tools
package scripting

import java.io.File
import java.nio.file.{Path, Paths, Files}
import scala.sys.process._

import org.junit.Test

import vulpix.TestConfiguration

import dotty.tools.dotc.config.Properties._

/** Verifies correct handling of command line arguments by `dist/bin/scala` and `dist/bin/scalac`.
 *   +. arguments following a script path must be treated as script arguments
 *   +. preserve script command line arguments.
 */
class BashScriptsTests:
  // classpath tests managed by scripting.ClasspathTests.scala
  def testFiles = scripts("/scripting")

  printf("osname[%s]\n", osname)
  printf("using JAVA_HOME=%s\n", javaHome)
  printf("using SCALA_HOME=%s\n", scalaHome)
  printf("first 5 PATH entries:\n%s\n", pathEntries.take(5).mkString("\n"))
  printf("scala path:  [%s]\n", scalaPath)
  printf("scalac path: [%s]\n", scalacPath)

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
  val showArgsScript = testFiles.find(_.getName == "showArgs.sc").get.absPath

  /* verify `dist/bin/scalac` non-interference with command line args following script name */
  @Test def verifyScalacArgs =
    val commandline = (Seq(scalacPath, "-script", showArgsScript) ++ testScriptArgs).mkString(" ")
    val (validTest, exitCode, stdout, stderr) = bashCommand(commandline)
    if validTest then
      var fail = false
      printf("\n")
      for (line, expect) <- stdout zip expectedOutput do
        printf("expected: %-17s\nactual  : %s\n", expect, line)
        if line != expect then
          fail = true

      if fail then
        assert(stdout == expectedOutput)

  /* verify `dist/bin/scala` non-interference with command line args following script name */
  @Test def verifyScalaArgs =
    val commandline = (Seq(scalaPath, showArgsScript) ++ testScriptArgs).mkString(" ")
    val (validTest, exitCode, stdout, stderr) = bashCommand(commandline)
    if validTest then
      var fail = false
      printf("\n")
      var mismatches = List.empty[(String, String)]
      for (line, expect) <- stdout zip expectedOutput do
        printf("expected: %-17s\nactual  : %s\n", expect, line)
        if line != expect then
          fail = true

      if fail then
        assert(stdout == expectedOutput)

  /*
   * verify that scriptPath.sc sees a valid script.path property, 
   * and that it's value is the path to "scriptPath.sc".
   */
  @Test def verifyScriptPathProperty =
    val scriptFile = testFiles.find(_.getName == "scriptPath.sc").get
    val expected = s"/${scriptFile.getName}"
    System.err.printf("===> verify valid system property script.path is reported by script [%s]\n", scriptFile.getName)
    val (exitCode, stdout, stderr) = bashCommand(scriptFile.absPath)
    if exitCode == 0 && ! stderr.exists(_.contains("Permission denied")) then
      // var cmd = Array(bashExe, "-c", scriptFile.absPath)
      // val stdout = Process(cmd).lazyLines_!
      stdout.foreach { System.err.printf("######### [%s]\n", _) }
      val valid = stdout.exists { _.endsWith(expected) }
      if valid then printf("# valid script.path reported by [%s]\n", scriptFile.getName)
      assert(valid, s"script ${scriptFile.absPath} did not report valid script.path value")

  /*
   * verify SCALA_OPTS can specify an @argsfile when launching a scala script in `dist/bin/scala`.
   */
  @Test def verifyScalaOpts =
    val scriptFile = testFiles.find(_.getName == "classpathReport.sc").get
    printf("===> verify SCALA_OPTS -classpath setting in argument file seen by script [%s]\n", scriptFile.getName)
    val argsfile = createArgsFile() // avoid problems caused by drive letter
    val envPairs = List(("SCALA_OPTS", s"@$argsfile"))
    val (exitCode, stdout, stderr) = bashCommand(scriptFile.absPath, envPairs:_*)
    printf("\n")
    if exitCode != 0 || stderr.exists(_.contains("Permission denied")) then
      stderr.foreach { System.err.printf("stderr [%s]\n", _) }
      printf("unable to execute script, return value is %d\n", exitCode)
    else
      val expected = cwd
      val List(line1: String, line2: String) = stdout.take(2)
      printf("line1 [%s]\n", line1)
      val valid = line2.dropWhile( _ != ' ').trim.startsWith(expected)
      val psep = if osname.startsWith("Windows") then ';' else ':'
      printf("line2 start [%s]\n", line2.take(100))
      if valid then printf(s"\n===> success: classpath begins with %s, as reported by [%s]\n", cwd, scriptFile.getName)
      assert(valid, s"script ${scriptFile.getName} did not report valid java.class.path first entry")

  lazy val cwd: String = Paths.get(".").toAbsolutePath.normalize.toString.norm

  lazy val argsfile = createArgsFile() // avoid problems caused by drive letter
  def createArgsFile(): String =
    val utfCharset = java.nio.charset.StandardCharsets.UTF_8.name
    val text = s"-classpath $cwd"
    val path = Files.createTempFile("scriptingTest", ".args")
    val text = s"-classpath ${workingDirectory.absPath}"
    Files.write(path, text.getBytes(utfCharset))
    path.toFile.getAbsolutePath.norm

  extension(str: String)
    def norm: String = str.replace('\\', '/')
    def dropExtension: String = str.reverse.dropWhile(_ != '.').drop(1).reverse

  extension(f: File) def absPath: String =
    f.getAbsolutePath.norm

  // use optional TEST_BASH if defined, otherwise, bash must be in PATH
  lazy val bashExe: String = envOrElse("TEST_BASH", whichBash)

  // test env SCALA_HOME is:
  //    dist/target/pack, if present
  //    else, SCALA_HOME if defined
  //    else, not defined
  lazy val scalaHome =
    if scalacPath.isFile then scalacPath.replaceAll("/bin/scalac","")
    else envOrElse("SCALA_HOME", "").norm

  lazy val javaHome = envOrElse("JAVA_HOME", "").norm

  lazy val testEnvPairs = List(
    ("JAVA_HOME", javaHome),
    ("SCALA_HOME", scalaHome),
    ("PATH", adjustedPath),
  ).filter { case (name,valu) => valu.nonEmpty }

  lazy val whichBash: String =
    var whichBash = ""
    if osname.startsWith("windows") then
      whichBash = which("bash.exe")
    else
      whichBash = which("bash")

    whichBash

  def bashCommand(cmdstr: String, additionalEnvPairs:List[(String, String)] = Nil): (Boolean, Int, Seq[String], Seq[String]) = {
    var (stdout, stderr) = (List.empty[String], List.empty[String])
    if bashExe.toFile.exists then
      val cmd = Seq(bashExe, "-c", cmdstr)
      val envPairs = testEnvPairs ++ additionalEnvPairs
      val proc = Process(cmd, None, envPairs *)
      val exitVal = proc ! ProcessLogger (
        (out: String) => stdout ::= out, 
        (err: String) => stderr ::= err
      )
      val validTest = exitVal == 0 && ! stderr.exists(_.contains("Permission denied"))
      if ! validTest then
        printf("\nunable to execute script, return value is %d\n", exitVal)
        stderr.foreach { System.err.printf("stderr [%s]\n", _) }
      (validTest, exitVal, stdout.reverse, stderr.reverse)
    else
      (false, -1, Nil, Nil)
  }

  def execCmd(command: String, options: String *): Seq[String] =
    val cmd = (command :: options.toList).toSeq
    for {
      line <- Process(cmd).lazyLines_!
    } yield line
