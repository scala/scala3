package dotty
package tools
package scripting

import org.junit.Test
import org.junit.Assert.assertEquals

import vulpix.TestConfiguration

import ScriptTestEnv.*

/** Verifies correct handling of command line arguments by `dist/bin/scala` and `dist/bin/scalac`.
 *   +. arguments following a script path must be treated as script arguments
 *   +. preserve script command line arguments.
 *   +. prevent SCALA_OPTS in build environment from infecting tests, via 'SCALA_OPTS= ' prefix
 *   +. test scripts must not throw execptions or exit with nonzero.
 */
class BashScriptsTests:
  // classpath tests managed by scripting.ClasspathTests.scala
  def testFiles = scripts("/scripting")
  lazy val argsfile = createArgsFile() // avoid problems caused by drive letter

  printf("osname[%s]\n", osname)
  printf("using JAVA_HOME=%s\n", envJavaHome)
  printf("using SCALA_HOME=%s\n", envScalaHome)
  printf("first 5 PATH entries:\n%s\n", adjustedPathEntries.take(5).mkString("\n"))
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
    val commandline = (Seq("SCALA_OPTS= ", scalacPath, "-script", showArgsScript) ++ testScriptArgs).mkString(" ")
    val (validTest, exitCode, stdout, stderr) = bashCommand(commandline)
    if verifyValid(validTest) then
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
    val commandline = Seq("SCALA_OPTS= ", scalaPath, "-J-Dkey=World", testFiles.find(_.getName == "envtest.sc").get.absPath).mkString(" ")
    val (validTest, exitCode, stdout, stderr) = bashCommand(commandline)
    assertEquals(stdout.mkString("/n"), "Hello World")

  /* verify `dist/bin/scala` with -J setting */
  @Test def verifyScalaJProperty =
    val commandline = Seq("SCALA_OPTS= ", scalaPath, "-J-Dkey=World3", testFiles.find(_.getName == "envtest.scala").get.absPath).mkString(" ")
    val (validTest, exitCode, stdout, stderr) = bashCommand(commandline)
    assertEquals(stdout.mkString("/n"), "Hello World3")

  /* verify `dist/bin/scala` with -D setting */
  @Test def verifyScDProperty =
    val commandline = Seq("SCALA_OPTS= ", scalaPath, "-Dkey=World3", testFiles.find(_.getName == "envtest.sc").get.absPath).mkString(" ")
    val (validTest, exitCode, stdout, stderr) = bashCommand(commandline)
    assertEquals(stdout.mkString("/n"), "Hello World3")

  /* verify `dist/bin/scala` with -D setting */
  @Test def verifyScalaDProperty =
    val commandline = Seq("SCALA_OPTS= ", scalaPath, "-Dkey=World4", testFiles.find(_.getName == "envtest.scala").get.absPath).mkString(" ")
    val (validTest, exitCode, stdout, stderr) = bashCommand(commandline)
    assertEquals(stdout.mkString("/n"), "Hello World4")

  /* verify `dist/bin/scala` with -D setting */
  @Test def saveAndRunWithDProperty =
    val commandline = Seq("SCALA_OPTS= ", scalaPath, "-save", testFiles.find(_.getName == "envtest.scala").get.absPath).mkString(" ")
    val (_, _, _, _) = bashCommand(commandline)
    val commandline2 = Seq("SCALA_OPTS= ", scalaPath, "-Dkey=World5", testFiles.find(_.getName == "envtest.jar").get.absPath).mkString(" ")
    val (validTest, exitCode, stdout, stderr) = bashCommand(commandline2)
    assertEquals(stdout.mkString("/n"), "Hello World5")

  /* verify `dist/bin/scala` non-interference with command line args following script name */
  @Test def verifyScalaArgs =
    val commandline = (Seq("SCALA_OPTS= ", scalaPath, showArgsScript) ++ testScriptArgs).mkString(" ")
    val (validTest, exitCode, stdout, stderr) = bashCommand(commandline)
    if verifyValid(validTest) then
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
    if verifyValid(validTest) then
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
    printf("===> verify SCALA_OPTS='@argsfile' is properly handled by `dist/bin/scala`\n")
    val envPairs = List(("SCALA_OPTS", s"@$argsfile"))
    val (validTest, exitCode, stdout, stderr) = bashCommand(scriptFile.absPath, envPairs)
    printf("stdout: %s\n", stdout.mkString("\n","\n",""))
    if verifyValid(validTest) then
      val expected = s"${workingDirectory.norm}"
      val output = stdout.find( _.trim.startsWith("cwd") ).getOrElse("").dropWhile(_!=' ').trim
      printf("output  [%s]\n", output)
      printf("expected[%s]\n", expected)
      val valid = output.startsWith(expected)
      if valid then printf(s"\n===> success: classpath begins with %s, as reported by [%s]\n", workingDirectory, scriptFile.getName)
      assert(valid, s"script ${scriptFile.absPath} did not report valid java.class.path first entry")

