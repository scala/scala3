package dotty
package tools
package scripting

import org.junit.{Test, AfterClass}
import org.junit.Assert.assertEquals

import vulpix.TestConfiguration

import ScriptTestEnv.*

/** Verifies correct handling of command line arguments by `dist/bin/scala` and `dist/bin/scalac`.
 *   +. arguments following a script path must be treated as script arguments
 *   +. preserve script command line arguments.
 *   +. prevent SCALA_OPTS in build environment from infecting tests, via 'SCALA_OPTS= ' prefix
 *   +. test scripts must not throw execptions or exit with nonzero.
 */
object BashScriptsTests:
  lazy val argsfile = createArgsFile() // avoid problems caused by drive letter
  lazy val testFiles = scripts("/scripting")

  @AfterClass def cleanup: Unit = {
    val af = argsfile.toFile
    if (af.exists) {
      af.delete()
    }
  }
  printf("osname[%s]\n", osname)
  printf("uname[%s]\n", ostypeFull)
  printf("using JAVA_HOME=%s\n", envJavaHome)
  printf("using SCALA_HOME=%s\n", envScalaHome)
  printf("first 5 PATH entries:\n%s\n", adjustedPathEntries.take(5).mkString("\n"))
  printf("scala path:  [%s]\n", scalaPath)
  printf("scalac path: [%s]\n", scalacPath)

  val expectedOutput = List(
    "arg  0:[a]",
    "arg  1:[b]",
    "arg  2:[c]",
    "arg  3:[-repl]",
    "arg  4:[-run]",
    "arg  5:[-script]",
    "arg  6:[-debug]",
  )
  val testScriptArgs = Seq(
    "a", "b", "c", "-repl", "-run", "-script", "-debug"
  )
  val showArgsScript = testFiles.find(_.getName == "showArgs.sc").get.absPath

  def testFile(name: String): String = 
    val file = testFiles.find(_.getName == name) match {
      case Some(f) =>
        val ff = f.absPath
        printf("test file [%s] is [%s]\n", name, ff)
        ff
      case None =>
        printf("test file [%s] not found!\n", name)
        name.absPath
    }
    file

  val Seq(envtestSc, envtestScala) = Seq("envtest.sc", "envtest.scala").map { testFile(_) }

  // create command line with given options, execute specified script, return stdout
  def callScript(tag: String, script: String, keyPre: String): String =
    val keyArg = s"$keyPre=$tag"
    printf("pass tag [%s] via [%s] to script [%s]\n", tag, keyArg, script)
    val cmd: String = Seq("SCALA_OPTS= ", scalaPath, keyArg, script).mkString(" ")
    printf("cmd: [%s]\n", cmd)
    val (validTest, exitCode, stdout, stderr) = bashCommand(cmd)
    stderr.filter { !_.contains("Inappropriate ioctl") }.foreach { System.err.printf("stderr [%s]\n", _) }
    stdout.mkString("\n")


class BashScriptsTests:
  import BashScriptsTests.*
  // classpath tests managed by scripting.ClasspathTests.scala

  ////////////////////////// begin tests //////////////////////
  
  /* verify that `dist/bin/scala` correctly passes args to the jvm via -J-D for script envtest.sc */
  @Test def verifyScJProperty =
    val tag = "World1"
    val stdout = callScript(tag, envtestSc, s"-J-Dkey")
    assertEquals( s"Hello $tag", stdout)

  /* verify that `dist/bin/scala` correctly passes args to the jvm via -J-D for script envtest.scala */
  @Test def verifyScalaJProperty =
    val tag = "World2"
    val stdout = callScript(tag, envtestScala, s"-J-Dkey")
    assertEquals(s"Hello $tag", stdout)

  /* verify that `dist/bin/scala` can set system properties via -D for envtest.sc */
  @Test def verifyScDProperty =
    val tag = "World3"
    val stdout = callScript(tag, envtestSc, s"-Dkey")
    assertEquals(s"Hello $tag", stdout)

  /* verify that `dist/bin/scala` can set system properties via -D for envtest.scala */
  @Test def verifyScalaDProperty =
    val tag = "World4"
    val stdout = callScript(tag, envtestScala, s"-Dkey")
    assertEquals(s"Hello $tag", stdout)

  /* verify that `dist/bin/scala` can set system properties via -D when executing compiled script via -jar envtest.jar */
  @Test def saveAndRunWithDProperty =
    val commandline2 = Seq("SCALA_OPTS= ", scalaPath.relpath, s"-Dkey=$tag", testJar.relpath)
    val commandline = Seq("SCALA_OPTS= ", scalaPath.relpath, "-save", envtestScala.relpath).mkString(" ")
    val (_, _, _, _) = bashCommand(commandline) // compile jar, discard output
    val testJar = testFile("envtest.jar") // jar is created by the previous bashCommand()
    if (testJar.isFile){
      printf("compiled envtest.scala to %s\n", testJar.norm)
    } else {
      sys.error(s"error: unable to compile envtest.scala to ${testJar.norm}")
    }

    val tag = "World5"
    val commandline2 = Seq("SCALA_OPTS= ", scalaPath.relpath, s"-Dkey=$tag", testJar.relpath)
    printf("cmd[%s]\n", commandline2.mkString(" "))
    val (validTest, exitCode, stdout, stderr) = bashCommand(commandline2.mkString(" "))
    assertEquals(s"Hello $tag", stdout.mkString("/n"))

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

  /* verify `dist/bin/scala` non-interference with command line args following script name */
  @Test def verifyScalaArgs =
    val commandline = (Seq("SCALA_OPTS= ", scalaPath, showArgsScript) ++ testScriptArgs).mkString(" ")
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
      // stdout might be polluted with an ANSI color prefix, so be careful
      val cwdline = stdout.find( _.trim.matches(".*cwd: .*") ).getOrElse("")
      printf("cwdline  [%s]\n", cwdline)
      printf("expected[%s]\n", expected)
      val valid = cwdline.endsWith(expected)
      if (!valid) then
        stdout.foreach { printf("stdout[%s]\n", _) }
        stderr.foreach { printf("stderr[%s]\n", _) }
      if valid then printf(s"\n===> success: classpath begins with %s, as reported by [%s]\n", workingDirectory, scriptFile.getName)
      assert(valid, s"script ${scriptFile.absPath} did not report valid java.class.path first entry")

