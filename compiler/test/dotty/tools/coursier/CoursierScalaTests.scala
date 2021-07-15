package dotty
package tools
package scripting

import java.io.File
import java.nio.file.{Path, Paths, Files}
import scala.sys.process._

import org.junit.Test
import org.junit.BeforeClass

import vulpix.TestConfiguration

import dotty.tools.absPath
import scala.collection.mutable.ListBuffer

class CoursierScalaTests:

  // classpath tests are managed by scripting.ClasspathTests.scala
  def testFiles = scripts("/scripting").filter { ! _.getName.startsWith("classpath") }

  // Cannot run tests in parallel, more info here: https://stackoverflow.com/questions/6345660/java-executing-bash-script-error-26-text-file-busy
  @Test def allTests =
    def scriptArgs() =
      val scriptPath = scripts("/scripting").find(_.getName == "showArgs.sc").get.absPath
      val testScriptArgs = Seq("a", "b", "c", "-repl", "-run", "-script", "-debug")

      val args = scriptPath +: testScriptArgs
      val output = CoursierScalaTests.csCmd(args*)
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
      assert(output == expectedOutput)
    scriptArgs()

    def version() =
      val output = CoursierScalaTests.csCmd("-version")
      assert(output.mkString("\n").contains(sys.env("DOTTY_BOOTSTRAPPED_VERSION")))
    version()

    def emptyArgsEqualsRepl() =
      val output = CoursierScalaTests.csCmd()
      assert(output.mkString("\n").contains("Unable to create a system terminal")) // Scala attempted to create REPL so we can assume it is working
    emptyArgsEqualsRepl()

    def repl() =
      val output = CoursierScalaTests.csCmd("-repl")
      assert(output.mkString("\n").contains("Unable to create a system terminal")) // Scala attempted to create REPL so we can assume it is working
    repl()


object CoursierScalaTests:

  def execCmd(command: String, options: String*): List[String] =
    val cmd = (command :: options.toList).toSeq.mkString(" ")
    val out = new ListBuffer[String]
    cmd.!(ProcessLogger(out += _, out += _))
    out.toList


  def csCmd(options: String*): List[String] =
    val newOptions = options match
      case Nil => options
      case _ => "--" +: options
    execCmd("./cs", (s"""launch "org.scala-lang:scala3-compiler_3:${sys.env("DOTTY_BOOTSTRAPPED_VERSION")}" --main-class "dotty.tools.MainGenericRunner" --property "scala.usejavacp=true"""" +: newOptions)*)

  /** Get coursier script */
  @BeforeClass def setup(): Unit =
    val ver = execCmd("uname").head.replace('L', 'l').replace('D', 'd')
    execCmd("curl", s"-fLo cs https://git.io/coursier-cli-$ver") #&& execCmd("chmod", "+x cs")

