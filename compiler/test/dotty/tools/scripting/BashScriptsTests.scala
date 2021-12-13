package dotty
package tools
package scripting

import java.io.File
import java.nio.file.{Path, Paths, Files}
import scala.sys.process._

import org.junit.Test
import org.junit.Assert.assertEquals

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

  /* verify `dist/bin/scala` with -J setting */
  @Test def verifyScJProperty =
    val commandline = Seq(scalaPath, "-J-Dkey=World", testFiles.find(_.getName == "envtest.sc").get.absPath).mkString(" ")
    val (validTest, exitCode, stdout, stderr) = bashCommand(commandline)
    assertEquals(stdout.mkString("/n"), "Hello World")

  /* verify `dist/bin/scala` with -J setting */
  @Test def verifyScalaJProperty =
    val commandline = Seq(scalaPath, "-J-Dkey=World3", testFiles.find(_.getName == "envtest.scala").get.absPath).mkString(" ")
    val (validTest, exitCode, stdout, stderr) = bashCommand(commandline)
    assertEquals(stdout.mkString("/n"), "Hello World3")

  /* verify `dist/bin/scala` with -D setting */
  @Test def verifyScDProperty =
    val commandline = Seq(scalaPath, "-Dkey=World3", testFiles.find(_.getName == "envtest.sc").get.absPath).mkString(" ")
    val (validTest, exitCode, stdout, stderr) = bashCommand(commandline)
    assertEquals(stdout.mkString("/n"), "Hello World3")

  /* verify `dist/bin/scala` with -D setting */
  @Test def verifyScalaDProperty =
    val commandline = Seq(scalaPath, "-Dkey=World4", testFiles.find(_.getName == "envtest.scala").get.absPath).mkString(" ")
    val (validTest, exitCode, stdout, stderr) = bashCommand(commandline)
    assertEquals(stdout.mkString("/n"), "Hello World4")

  /* verify `dist/bin/scala` with -D setting */
  @Test def saveAndRunWithDProperty =
    val commandline = Seq(scalaPath, "-save", testFiles.find(_.getName == "envtest.scala").get.absPath).mkString(" ")
    val (_, _, _, _) = bashCommand(commandline)
    val commandline2 = Seq(scalaPath, "-Dkey=World5", testFiles.find(_.getName == "envtest.jar").get.absPath).mkString(" ")
    val (validTest, exitCode, stdout, stderr) = bashCommand(commandline2)
    assertEquals(stdout.mkString("/n"), "Hello World5")

  /* verify `dist/bin/scala` non-interference with command line args following script name */
  @Test def verifyScalaArgs =
    val commandline = (Seq("SCALA_OPTS= ", scalaPath, showArgsScript) ++ testScriptArgs).mkString(" ")
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
    val expected = s"${scriptFile.getName}"
    printf("===> verify valid system property script.path is reported by script [%s]\n", scriptFile.getName)
    printf("calling scriptFile: %s\n", scriptFile)
    val (validTest, exitCode, stdout, stderr) = bashCommand(scriptFile.absPath)
    if validTest then
      stdout.foreach { printf("stdout: [%s]\n", _) }
      stderr.foreach { printf("stderr: [%s]\n", _) }
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
    val (validTest, exitCode, stdout, stderr) = bashCommand(scriptFile.absPath, envPairs)
    printf("stdout: %s\n", stdout.mkString("\n","\n",""))
    if validTest then
      val expected = s"${workingDirectory.norm}"
      val output = stdout.find( _.trim.startsWith("cwd") ).getOrElse("").dropWhile(_!=' ').trim
      printf("output  [%s]\n", output)
      printf("expected[%s]\n", expected)
      val valid = output.startsWith(expected)
      if valid then printf(s"\n===> success: classpath begins with %s, as reported by [%s]\n", workingDirectory, scriptFile.getName)
      assert(valid, s"script ${scriptFile.absPath} did not report valid java.class.path first entry")

  def existingPath: String = envOrElse("PATH", "").norm
  def adjustedPath = s"$javaHome/bin$psep$scalaHome/bin$psep$existingPath"
  def pathEntries = adjustedPath.split(psep).toList

  lazy val argsfile = createArgsFile() // avoid problems caused by drive letter
  def createArgsFile(): String =
    val utfCharset = java.nio.charset.StandardCharsets.UTF_8.name
    val text = s"-classpath $cwd"
    val path = Files.createTempFile("scriptingTest", ".args")
    val text = s"-classpath ${workingDirectory.absPath}"
    Files.write(path, text.getBytes(utfCharset))
    path.toFile.getAbsolutePath.norm

  def fixHome(s: String): String =
    s.startsWith("~") match {
    case false => s
    case true => s.replaceFirst("~", userHome)
    }

  extension(s: String) {
    def toPath: Path = Paths.get(fixHome(s)) // .toAbsolutePath
    def toFile: File = s.toPath.toFile
    def absPath: String = s.toFile.absPath
    def norm: String = s.replace('\\', '/') // bash expects forward slash
    def isFile: Boolean = s.toFile.isFile
    def exists: Boolean = s.toPath.toFile.exists
    def name: String = s.toFile.getName
    def dropExtension: String = s.reverse.dropWhile(_ != '.').drop(1).reverse
    def parent(up: Int): String = s.norm.split("/").reverse.drop(up).reverse.mkString("/")
  }

  extension(p: Path) {
    def listFiles: Seq[File] = p.toFile.listFiles.toList
    def norm: String = p.normalize.toString.replace('\\', '/')
    def name: String = p.toFile.getName
  }

  extension(f: File) {
    def name = f.getName
    def norm: String = f.toPath.normalize.norm
    def absPath: String = f.getAbsolutePath.norm
  }

  lazy val psep: String = propOrElse("path.separator", "")
  lazy val osname = propOrElse("os.name", "").toLowerCase

  lazy val scalacPath = s"$workingDirectory/dist/target/pack/bin/scalac".norm
  lazy val scalaPath = s"$workingDirectory/dist/target/pack/bin/scala".norm

  extension(f: File) def absPath: String =
    f.getAbsolutePath.norm

  // use optional TEST_BASH if defined, otherwise, bash must be in PATH
  lazy val bashExe: String = envOrElse("TEST_BASH", whichBash)

  // test env SCALA_HOME is:
  //    dist/target/pack, if present
  //    else, SCALA_HOME if defined
  //    else, not defined
  lazy val scalaHome =
    if scalacPath.isFile then scalacPath.replaceAll("/bin/scalac", "")
    else envOrElse("SCALA_HOME", "").norm

  lazy val javaHome = whichJava.parent(2)

  lazy val testEnvPairs = List(
    ("JAVA_HOME", javaHome),
    ("SCALA_HOME", scalaHome),
    ("PATH", adjustedPath),
  ).filter { case (name, valu) => valu.nonEmpty }

  lazy val whichBash: String = whichExe("bash")
  lazy val whichJava: String = whichExe("java")

  def whichExe(basename: String): String = 
    val exeName = if (osname.toLowerCase.startsWith("windows")) s"$basename.exe" else basename
    which(exeName)

  def bashCommand(cmdstr: String, additionalEnvPairs: List[(String, String)] = Nil): (Boolean, Int, Seq[String], Seq[String]) = {
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
