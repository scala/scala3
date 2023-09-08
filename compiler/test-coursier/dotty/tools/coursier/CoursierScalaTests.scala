package dotty
package tools
package coursier

import scala.language.unsafeNulls

import java.io.File
import java.nio.file.{Path, Paths, Files}
import scala.sys.process._
import org.junit.Test
import org.junit.BeforeClass
import org.junit.Assert._
import scala.collection.mutable.ListBuffer

import java.net.URLClassLoader
import java.net.URL

class CoursierScalaTests:

  private def scripts(path: String): Array[File] = {
    val dir = new File(getClass.getResource(path).getPath)
    assert(dir.exists && dir.isDirectory, "Couldn't load scripts dir")
    dir.listFiles
  }

  extension (f: File) private def absPath =
    f.getAbsolutePath.replace('\\', '/')

  extension (str: String) private def dropExtension =
    str.reverse.dropWhile(_ != '.').drop(1).reverse

  // classpath tests are managed by scripting.ClasspathTests.scala
  def testFiles = scripts("/scripting").filter { ! _.getName.startsWith("classpath") }

  // Cannot run tests in parallel, more info here: https://stackoverflow.com/questions/6345660/java-executing-bash-script-error-26-text-file-busy
  @Test def allTests =
    def scriptArgs() =
      val scriptPath = scripts("/scripting").find(_.getName == "showArgs.sc").get.absPath
      val testScriptArgs = Seq("a", "b", "c", "-repl", "-run", "-script", "-debug")

      val args = scriptPath +: testScriptArgs
      val output = CoursierScalaTests.csScalaCmd(args*)
      val expectedOutput = List(
        "arg  0:[a]",
        "arg  1:[b]",
        "arg  2:[c]",
        "arg  3:[-repl]",
        "arg  4:[-run]",
        "arg  5:[-script]",
        "arg  6:[-debug]",
      )
      for (line, expect) <- output zip expectedOutput do
        printf("expected: %-17s\nactual  : %s\n", expect, line)
      assertEquals(expectedOutput, output)
    scriptArgs()

    def scriptPath() =
      val scriptPath = scripts("/scripting").find(_.getName == "scriptPath.sc").get.absPath
      val args = scriptPath
      val output = CoursierScalaTests.csScalaCmd(args)
      assertTrue(output.mkString("\n").startsWith("script.path:"))
      assertTrue(output.mkString("\n").endsWith("scriptPath.sc"))
    scriptPath()

    def scriptEnvDashJDashD() =
      val scriptPath = scripts("/scripting").find(_.getName == "envtest.sc").get.absPath
      val args = scriptPath
      val output = CoursierScalaTests.csScalaCmd("-J-Dkey=World", args)
      assertEquals(output.mkString("\n"), "Hello World")
    scriptEnvDashJDashD()

    def version() =
      val output = CoursierScalaTests.csScalaCmd("-version")
      assertTrue(output.mkString("\n").contains(sys.env("DOTTY_BOOTSTRAPPED_VERSION")))
    version()

    def emptyArgsEqualsRepl() =
      val output = CoursierScalaTests.csScalaCmd()
      assertTrue(output.mkString("\n").contains("Unable to create a system terminal")) // Scala attempted to create REPL so we can assume it is working
    emptyArgsEqualsRepl()

    def run() =
      val output = CoursierScalaTests.csScalaCmd("-classpath", scripts("/run").head.getParentFile.getParent, "-run", "run.myfile")
      assertEquals(output.mkString("\n"), "Hello")
    run()

    def runDashJDashD() =
      val output = CoursierScalaTests.csScalaCmd("-J-Dkey=World", "-classpath", scripts("/run").head.getParentFile.getParent, "-run", "run.envtest")
      assertEquals(output.mkString("\n"), "Hello World")
    runDashJDashD()

    def notOnlyOptionsEqualsRun() =
      val output = CoursierScalaTests.csScalaCmd("-classpath", scripts("/run").head.getParentFile.getParent, "run.myfile")
      assertEquals(output.mkString("\n"), "Hello")
    notOnlyOptionsEqualsRun()

    def help() =
      val output = CoursierScalaTests.csScalaCmd("-help")
      assertTrue(output.mkString("\n").contains("Usage: scala <options> <source files>"))
    help()

    def jar() =
      val source = new File(getClass.getResource("/run/myfile.scala").getPath)
      val output = CoursierScalaTests.csScalaCmd("-save", source.absPath)
      assertEquals(output.mkString("\n"), "Hello")
      assertTrue(source.getParentFile.listFiles.find(_.getName == "myfile.jar").isDefined)
    jar()

    def runThatJar() =
      val source = new File(getClass.getResource("/run/myfile.jar").getPath)
      val output = CoursierScalaTests.csScalaCmd(source.absPath)
      assertEquals(output.mkString("\n"), "Hello")
    runThatJar()

    def compileFilesToJarAndRun() =
      val source = new File(getClass.getResource("/run/myfile.scala").getPath)
      val prefix = source.getParent

      val o1source = Paths.get(prefix, "automain.jar").toAbsolutePath.toString
      val output1 = CoursierScalaTests.csScalaCompilerCmd("-d", o1source, source.absPath)
      assertEquals(output1.mkString("\n"), "")

      val o2source = Paths.get(prefix, "custommain.jar").toAbsolutePath.toString
      val output2 = CoursierScalaTests.csScalaCompilerCmd("-d", o2source, "-Xmain-class", "run.myfile", source.absPath)
      assertEquals(output2.mkString("\n"), "")

      val output3 = CoursierScalaTests.csScalaCmd(o1source)
      assertEquals(output3.mkString("\n"), "Hello")

      val output4 = CoursierScalaTests.csScalaCmd(o2source)
      assertEquals(output4.mkString("\n"), "Hello")
    compileFilesToJarAndRun()

    def replWithArgs() =
      val output = CoursierScalaTests.csScalaCmd("-source", "3.0-migration")
      assertTrue(output.mkString("\n").contains("Unable to create a system terminal")) // Scala attempted to create REPL so we can assume it is working
    replWithArgs()

    def argumentFile() =
      // verify that an arguments file is accepted
      // verify that setting a user classpath does not remove compiler libraries from the classpath.
      // arguments file contains "-classpath .", adding current directory to classpath.
      val source = new File(getClass.getResource("/run/myfile.scala").getPath)
      val argsFile = new File(getClass.getResource("/run/myargs.txt").getPath)
      val output = CoursierScalaTests.csScalaCmd(s"@$argsFile", source.absPath)
      assertEquals(output.mkString("\n"), "Hello")
    argumentFile()

object CoursierScalaTests:

  def execCmd(command: String, options: String*): (Int, List[String]) =
    val cmd = (command :: options.toList).toSeq.mkString(" ")
    val out = new ListBuffer[String]
    val code = cmd.!(ProcessLogger(out += _, out += _))
    (code, out.toList)

  def csScalaCmd(options: String*): List[String] =
    csCmd("dotty.dist.MainGenericRunner", options*)

  def csScalaCompilerCmd(options: String*): List[String] =
    csCmd("dotty.tools.dotc.Main", options*)

  private def csCmd(entry: String, options: String*): List[String] =
    val (jOpts, args) = options.partition(_.startsWith("-J"))
    val newOptions = args match
      case Nil => args
      case _ => "--" +: args
    val newJOpts = jOpts.map(s => s"--java-opt ${s.stripPrefix("-J")}").mkString(" ")
    execCmd("./cs", (s"""launch "org.scala-lang:scala3-compiler_3:${sys.env("DOTTY_BOOTSTRAPPED_VERSION")}" $newJOpts --main-class "$entry" --property "scala.usejavacp=true"""" +: newOptions)*)._2

  /** Get coursier script */
  @BeforeClass def setup(): Unit =
    val launcherLocation = "https://github.com/coursier/launchers/raw/master"
    val launcherName = execCmd("uname")._2.head.toLowerCase match
      case "linux" => "cs-x86_64-pc-linux"
      case "darwin" => "cs-x86_64-apple-darwin"
      case other => fail(s"Unsupported OS for coursier launcher: $other")

    def runAndCheckCmd(cmd: String, options: String*): Unit =
      val (code, out) = execCmd(cmd, options*)
      if code != 0 then
        fail(s"Failed to run $cmd ${options.mkString(" ")}, exit code: $code, output: ${out.mkString("\n")}")

    runAndCheckCmd("curl", s"-fLo cs $launcherLocation/$launcherName")
    runAndCheckCmd("chmod", "+x cs")
