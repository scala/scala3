import java.io.{ByteArrayOutputStream, PrintStream}

import dotty.tools.dotc.util.DiffUtil

import scala.io.Source
import scala.tasty.file._
import scala.tasty.interpreter.TastyInterpreter
import scala.tasty.Reflection

object Test {
  def main(args: Array[String]): Unit = {
    val ps = new ByteArrayOutputStream()
    try scala.Console.withOut(ps) {
        ConsumeTasty("", List("IntepretedMain", "InterpretedBar"), new TastyInterpreter)
    } catch {
      case e: Throwable => throw new Exception(ps.toString, e)
    }
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

    val actualOutput = ps.toString

    assert(expectedOutput == actualOutput,
      "\n>>>>>>>>>>>>>>>>>>\n" +
      DiffUtil.mkColoredCodeDiff(actualOutput, expectedOutput, true) +
      "<<<<<<<<<<<<<<<<<<"
    )
  }
}
