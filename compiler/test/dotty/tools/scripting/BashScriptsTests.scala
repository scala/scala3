package dotty
package tools
package scripting

import scala.language.unsafeNulls

import java.nio.file.Paths
import org.junit.{Test, Ignore, AfterClass}
import org.junit.Assert.assertEquals
import org.junit.Assume.assumeFalse
import org.junit.experimental.categories.Category

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
  def testFiles = scripts("/scripting")

  @AfterClass def cleanup: Unit = {
    cleanupScalaCLIDirs()

    val af = argsfile.toFile
    if af.exists then
      af.delete()
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
  val Seq(showArgsScript, showArgsScalaCli) = Seq("showArgs.sc", "showArgs_scalacli.sc").map { name =>
    testFiles.find(_.getName == name).get.absPath
  }

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

  val Seq(envtestNuSc, envtestScala) = Seq("envtest_scalacli.sc", "envtest.scala").map { testFile(_) }

  // create command line with given options, execute specified script, return stdout
  def callScript(tag: String, script: String, keyPre: String): String =
    val keyArg = s"$keyPre=$tag"
    printf("pass tag [%s] via [%s] to script [%s]\n", tag, keyArg, script)
    val cmd: String = Seq("SCALA_OPTS= ", scalaPath, "run", keyArg, "--power", "--offline", "--server=false", script).mkString(" ")
    printf("cmd: [%s]\n", cmd)
    val (validTest, exitCode, stdout, stderr) = bashCommand(cmd)
    stderr.filter { !_.contains("Inappropriate ioctl") }.foreach { System.err.printf("stderr [%s]\n", _) }
    stdout.mkString("\n")


@Ignore
class BashScriptsTests:
  import BashScriptsTests.*
  // classpath tests managed by scripting.ClasspathTests.scala

  ////////////////////////// begin tests //////////////////////

  /* verify that `dist/bin/scala` correctly passes args to the jvm via -J-D for script envtest.sc */
  @Ignore // SCALA CLI does not support `-J` to pass java properties, only things like -Xmx5g
  @Test def verifyScJProperty =
    assumeFalse("Scripts do not yet support Scala 2 library TASTy", Properties.usingScalaLibraryTasty)
    val tag = "World1"
    val stdout = callScript(tag, envtestNuSc, s"-J-Dkey")
    assertEquals( s"Hello $tag", stdout)

  /* verify that `dist/bin/scala` correctly passes args to the jvm via -J-D for script envtest.scala */
  @Ignore // SCALA CLI does not support `-J` to pass java properties, only things like -Xmx5g
  @Test def verifyScalaJProperty =
    assumeFalse("Scripts do not yet support Scala 2 library TASTy", Properties.usingScalaLibraryTasty)
    val tag = "World2"
    val stdout = callScript(tag, envtestScala, s"-J-Dkey")
    assertEquals(s"Hello $tag", stdout)

  /* verify that `dist/bin/scala` can set system properties via -D for envtest.sc */
  @Test def verifyScDProperty =
    assumeFalse("Scripts do not yet support Scala 2 library TASTy", Properties.usingScalaLibraryTasty)
    val tag = "World3"
    val stdout = callScript(tag, envtestNuSc, s"-Dkey")
    assertEquals(s"Hello $tag", stdout)

  /* verify that `dist/bin/scala` can set system properties via -D for envtest.scala */
  @Test def verifyScalaDProperty =
    assumeFalse("Scripts do not yet support Scala 2 library TASTy", Properties.usingScalaLibraryTasty)
    val tag = "World4"
    val stdout = callScript(tag, envtestScala, s"-Dkey")
    assertEquals(s"Hello $tag", stdout)

  /* verify that `dist/bin/scala` can set system properties via -D when executing compiled script via -jar envtest.jar */
  @Test def saveAndRunWithDProperty =
    assumeFalse("Scripts do not yet support Scala 2 library TASTy", Properties.usingScalaLibraryTasty)
    val libOut = envtestScala.relpath.stripSuffix(".scala") + ".jar"
    val commandline = Seq(
      "SCALA_OPTS= ", scalaPath.relpath, "--power", "package", envtestScala.relpath, "-o", libOut, "--library", "--offline", "--server=false").mkString(" ")
    val (_, _, _, _) = bashCommand(commandline) // compile jar, discard output
    val testJar = testFile("envtest.jar") // jar is created by the previous bashCommand()
    if (testJar.isFile){
      printf("compiled envtest.scala to %s\n", testJar.norm)
    } else {
      sys.error(s"error: unable to compile envtest.scala to ${testJar.norm}")
    }

    val tag = "World5"
    val commandline2 = Seq(
      "SCALA_OPTS= ", scalaPath.relpath, "run", s"-Dkey=$tag", "-classpath", testJar.relpath, "--power", "--offline", "--server=false")
    printf("cmd[%s]\n", commandline2.mkString(" "))
    val (validTest, exitCode, stdout, stderr) = bashCommand(commandline2.mkString(" "))
    assertEquals(s"Hello $tag", stdout.mkString("/n"))

  /* verify `dist/bin/scalac` non-interference with command line args following script name */
  @Test def verifyScalacArgs =
    assumeFalse("Scripts do not yet support Scala 2 library TASTy", Properties.usingScalaLibraryTasty)
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
    assumeFalse("Scripts do not yet support Scala 2 library TASTy", Properties.usingScalaLibraryTasty)
    val commandline = (
      Seq("SCALA_OPTS= ", scalaPath, showArgsScalaCli)
      ++ Seq("--power", "--offline", "--server=false")
      ++ ("--" +: testScriptArgs)
    ).mkString(" ")
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
   * verify that scriptPath_scalacli.sc sees a valid script.path property,
   * and that it's value is the path to "scriptPath_scalacli.sc".
   */
  @Category(Array(classOf[BootstrappedOnlyTests]))
  @Test def verifyScriptPathProperty =
    assumeFalse("Scripts do not yet support Scala 2 library TASTy", Properties.usingScalaLibraryTasty)
    val scriptFile = testFiles.find(_.getName == "scriptPath_scalacli.sc").get
    val expected = s"${scriptFile.getName}"
    printf("===> verify valid system property script.path is reported by script [%s]\n", scriptFile.getName)
    printf("calling scriptFile: %s\n", scriptFile)
    val (validTest, exitCode, stdout, stderr) = bashCommand(scriptFile.absPath)
    if verifyValid(validTest) then
      stdout.foreach { printf("stdout: [%s]\n", _) }
      stderr.foreach { printf("stderr: [%s]\n", _) }
      val valid = stdout.exists { _.endsWith(expected) }
      if valid then printf("# valid scriptPath reported by [%s]\n", scriptFile.getName)
      assert(valid, s"script ${scriptFile.absPath} did not report valid scriptPath value")

  /*
   * verify SCALA_OPTS can specify an @argsfile when launching a scala script in `dist/bin/scala`.
   */
  @Test def verifyScalaOpts =
    assumeFalse("Scripts do not yet support Scala 2 library TASTy", Properties.usingScalaLibraryTasty)
    val scriptFile = testFiles.find(_.getName == "classpathReport_scalacli.sc").get
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

  /*
   * verify that individual scripts can override -save with -nosave (needed to address #13760).
   */
  @Test def sqlDateTest =
    assumeFalse("Scripts do not yet support Scala 2 library TASTy", Properties.usingScalaLibraryTasty)
    val scriptBase = "sqlDateError_scalacli"
    val scriptFile = testFiles.find(_.getName == s"$scriptBase.sc").get
    val testJar = testFile(s"$scriptBase.jar") // jar should not be created when scriptFile runs
    val tj = Paths.get(testJar).toFile
    if tj.isFile then tj.delete() // discard residual debris from previous test
    printf("===> verify '-save' is cancelled by '-nosave' in script hashbang.`\n")
    val (validTest, exitCode, stdout, stderr) = bashCommand(s"SCALA_OPTS=-save ${scriptFile.absPath}")
    printf("stdout: %s\n", stdout.mkString("\n","\n",""))
    if verifyValid(validTest) then
      // the script should print '1969-12-31' or '1970-01-01', depending on time zone
      // stdout can be polluted with an ANSI color prefix, in some test environments
      val valid = stdout.mkString("").matches(""".*\d{4}-\d{2}-\d{2}.*""")
      if (!valid) then
        stdout.foreach { printf("stdout[%s]\n", _) }
        stderr.foreach { printf("stderr[%s]\n", _) }
      if valid then printf(s"\n===> success: scripts can override -save via -nosave\n")
      assert(valid, s"script ${scriptFile.absPath} reported unexpected value for java.sql.Date ${stdout.mkString("\n")}")
      assert(!testJar.exists,s"unexpected, jar file [$testJar] was created")


  /*
   * verify -e println("yo!") works.
   */
  @Test def verifyCommandLineExpression =
    assumeFalse("Scripts do not yet support Scala 2 library TASTy", Properties.usingScalaLibraryTasty)
    printf("===> verify -e <expression> is properly handled by `dist/bin/scala`\n")
    val expected = "9"
    val expression = s"println(3*3)"
    val (validTest, exitCode, stdout, stderr) = bashCommand(s"""bin/scala -e '$expression'""")
    val result = stdout.filter(_.nonEmpty).mkString("")
    printf("stdout: %s\n", result)
    printf("stderr: %s\n", stderr.mkString("\n","\n",""))
    if verifyValid(validTest) then
      assert(result.contains(expected), s"expression [$expression] did not send [$expected] to stdout")
