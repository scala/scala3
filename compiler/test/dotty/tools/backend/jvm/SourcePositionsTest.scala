package dotty.tools.backend.jvm

import scala.language.unsafeNulls

import org.junit.Assert._
import org.junit.Test

class SourcePositionsTest extends DottyBytecodeTest:
  import ASMConverters._

  @Test def issue18238_a(): Unit = {
    val code =
      """
      |class Test {
      |  def test(): Unit = {
      |    var x = 3
      |    var y = 2
      |    while(true) {
      |      if (x < y)
      |        if (x >= y)
      |          x += 1
      |        else
      |          y -= 1
      |    }
      |  }
      |}""".stripMargin

    checkBCode(code) { dir =>
      val testClass = loadClassNode(dir.lookupName("Test.class", directory = false).input, skipDebugInfo = false)
      val testMethod = getMethod(testClass, "test")
      val lineNumbers = instructionsFromMethod(testMethod).collect{case ln: LineNumber => ln}
      val expected = List(
        LineNumber(4, Label(0)),   // var x
        LineNumber(5, Label(4)),   // var y
        LineNumber(6, Label(8)),   // while(true)
        LineNumber(7, Label(13)),  // if (x < y)
        LineNumber(8, Label(18)),  //   if (x >= y)
        LineNumber(9, Label(23)),  //     x += 1
        LineNumber(11, Label(27)), //     y -= 1
        LineNumber(7, Label(32))   // <synthetic unit> point back to `if (x < y)
      )
      assertEquals(expected, lineNumbers)
    }
  }

  @Test  def issue18238_b(): Unit = {
    val code =
      """
      |class Test {
      |  def test(): Unit = {
      |    var x = 3
      |    var y = 2
      |    while(true) {
      |      if (x < y)
      |        if (x >= y)
      |          x += 1
      |        else
      |          y -= 1
      |      else ()
      |    }
      |  }
      |}""".stripMargin

    checkBCode(code) { dir =>
      val testClass = loadClassNode(dir.lookupName("Test.class", directory = false).input, skipDebugInfo = false)
      val testMethod = getMethod(testClass, "test")
      val lineNumbers = instructionsFromMethod(testMethod).collect{case ln: LineNumber => ln}
      val expected = List(
        LineNumber(4, Label(0)),   // var x
        LineNumber(5, Label(4)),   // var y
        LineNumber(6, Label(8)),   // while(true)
        LineNumber(7, Label(13)),  // if (x < y)
        LineNumber(8, Label(18)),  //   if (x >= y)
        LineNumber(9, Label(23)),  //     x += 1
        LineNumber(11, Label(27)), //     y -= 1
        LineNumber(12, Label(32))  // else ()
      )
      assertEquals(expected, lineNumbers)
    }
  }

  @Test  def issue18238_c(): Unit = {
    val code =
      """
      |class Test {
      |  def test(): Unit = {
      |    var x = 3
      |    var y = 2
      |    while(true) {
      |      if (x < y)
      |        if (x >= y)
      |          x += 1
      |        else
      |          y -= 1
      |      println()
      |    }
      |  }
      |}""".stripMargin

    checkBCode(code) { dir =>
      val testClass = loadClassNode(dir.lookupName("Test.class", directory = false).input, skipDebugInfo = false)
      val testMethod = getMethod(testClass, "test")
      val lineNumbers = instructionsFromMethod(testMethod).collect{case ln: LineNumber => ln}
      val expected = List(
        LineNumber(4, Label(0)),   // var x
        LineNumber(5, Label(4)),   // var y
        LineNumber(6, Label(8)),   // while(true)
        LineNumber(7, Label(13)),  // if (x < y)
        LineNumber(8, Label(18)),  //   if (x >= y)
        LineNumber(9, Label(23)),  //     x += 1
        LineNumber(11, Label(27)), //     y -= 1
        LineNumber(12, Label(31))  // println()
      )
      assertEquals(expected, lineNumbers)
    }
  }
