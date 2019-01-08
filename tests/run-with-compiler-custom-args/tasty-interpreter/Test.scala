import java.io.{ByteArrayOutputStream, File, PrintStream}

import dotty.tools.dotc.core.Contexts
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.dotc.reporting.diagnostic.MessageContainer
import dotty.tools.dotc.util.DiffUtil
import dotty.tools.io.Path

import scala.io.Source
import scala.tasty.file._
import scala.tasty.interpreter.TastyInterpreter
import scala.tasty.Reflection

object Test {
  def main(args: Array[String]): Unit = {

    val actualOutput = interpret("")("IntepretedMain", "InterpretedBar")
    val expectedOutput =
      """42
        |
        |Hello
        |
        |42
        |
        |43
        |
        |if
        |
        |5
        |4
        |3
        |2
        |1
        |
        |42
        |
        |55
        |precompiledModule
        |55
        |56
        |57
        |58
        |59
        |60
        |61
        |62
        |63
        |true
        |false
        |64
        |65
        |66
        |67
        |68
        |""".stripMargin

    assert(expectedOutput == actualOutput,
      "\n>>>>>>>>>>>>>>>>>>\n" +
      DiffUtil.mkColoredCodeDiff(actualOutput, expectedOutput, true) +
      "<<<<<<<<<<<<<<<<<<"
    )

//    compileAndInterpret("HelloWorld")
//    compileAndInterpret("i3518")
//    compileAndInterpret("withIndex")
  }

  def compileAndInterpret(testName: String) = {
    val reproter = new Reporter {
      def doReport(m: MessageContainer)(implicit ctx: Contexts.Context): Unit = println(m)
    }
    val out = java.nio.file.Paths.get("out/interpreted")
    if (!java.nio.file.Files.exists(out))
      java.nio.file.Files.createDirectory(out)
    dotty.tools.dotc.Main.process(Array("-classpath", System.getProperty("java.class.path"), "-d", out.toString, "tests/run/" + testName + ".scala"), reproter)

    val actualOutput = interpret(out.toString)("Test")

    val checkFile = java.nio.file.Paths.get("tests/run/" + testName + ".check")
    val expectedOutput = Source.fromFile(checkFile.toFile).getLines().mkString("\n")

    assert(expectedOutput == actualOutput,
      "\n>>>>>>>>>>>>>>>>>>\n" +
        DiffUtil.mkColoredCodeDiff(actualOutput, expectedOutput, true) +
        "<<<<<<<<<<<<<<<<<<"
    )
  }

  def interpret(classpath: String*)(interpretedClasses: String*): String = {
    val ps = new ByteArrayOutputStream()
    try scala.Console.withOut(ps) {
      ConsumeTasty(classpath.mkString(java.io.File.pathSeparatorChar.toString), interpretedClasses.toList, new TastyInterpreter)
    } catch {
      case e: Throwable => throw new Exception(ps.toString, e)
    }
    ps.toString
  }
}
