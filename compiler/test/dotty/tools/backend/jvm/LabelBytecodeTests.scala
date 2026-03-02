package dotty.tools.backend.jvm

import scala.language.unsafeNulls

import org.junit.Assert._
import org.junit.Test

import scala.tools.asm
import asm._
import asm.tree._

import scala.tools.asm.Opcodes
import scala.jdk.CollectionConverters._
import Opcodes._

class LabelBytecodeTests extends DottyBytecodeTest {
  import ASMConverters._

   @Test def localLabelBreak = {
    testLabelBytecodeEquals(
      """val local = boundary.Label[Long]()
        |try break(5L)(using local)
        |catch case ex: boundary.Break[Long] @unchecked =>
        |  if ex.isSameLabelAs(local) then ex.value
        |  else throw ex
      """.stripMargin,
      "Long",
      Ldc(LDC, 5),
      Op(LRETURN)
    )
  }

  @Test def simpleBoundaryBreak = {
    testLabelBytecodeEquals(
      """boundary: l ?=>
        |  break(2)(using l)
      """.stripMargin,
      "Int",
      Op(ICONST_2),
      Op(IRETURN)
    )

    testLabelBytecodeEquals(
      """boundary:
        |  break(3)
      """.stripMargin,
      "Int",
      Op(ICONST_3),
      Op(IRETURN)
    )

    testLabelBytecodeEquals(
      """boundary:
        |  break()
      """.stripMargin,
      "Unit",
      Op(RETURN)
    )
  }

  @Test def labelExtraction = {
    // Test extra Inlined around the label
    testLabelBytecodeEquals(
      """boundary:
        |  break(2)(using summon[boundary.Label[Int]])
      """.stripMargin,
      "Int",
      Op(ICONST_2),
      Op(IRETURN)
    )

    // Test extra Block around the label
    testLabelBytecodeEquals(
      """boundary: l ?=>
        |  break(2)(using { l })
      """.stripMargin,
      "Int",
      Op(ICONST_2),
      Op(IRETURN)
    )
  }

  @Test def boundaryLocalBreak = {
    testLabelBytecodeExpect(
      """val x: Boolean = true
        |boundary[Unit]:
        |  var i = 0
        |  while true do
        |    i += 1
        |    if i > 10 then break()
      """.stripMargin,
      "Unit",
      !throws(_)
    )
  }

  @Test def boundaryNonLocalBreak = {
    testLabelBytecodeExpect(
      """boundary[Unit]:
        |  nonLocalBreak()
      """.stripMargin,
      "Unit",
      throws
    )

    testLabelBytecodeExpect(
      """boundary[Unit]:
        |  def f() = break()
        |  f()
      """.stripMargin,
      "Unit",
      throws
    )
  }

  @Test def boundaryLocalAndNonLocalBreak = {
    testLabelBytecodeExpect(
      """boundary[Unit]: l ?=>
        |  break()
        |  nonLocalBreak()
      """.stripMargin,
      "Unit",
      throws
    )
  }

  private def throws(instructions: List[Instruction]): Boolean =
    instructions.exists {
      case Op(ATHROW) => true
      case _ => false
    }

  private def testLabelBytecodeEquals(code: String, tpe: String, expected: Instruction*): Unit =
    checkLabelBytecodeInstructions(code, tpe) { instructions =>
      val expectedList = expected.toList
      assert(instructions == expectedList,
        "`test` was not properly generated\n" + diffInstructions(instructions, expectedList))
    }

  private def testLabelBytecodeExpect(code: String, tpe: String, expected: List[Instruction] => Boolean): Unit =
    checkLabelBytecodeInstructions(code, tpe) { instructions =>
      assert(expected(instructions),
        "`test` was not properly generated\n" + instructions)
    }

   private def checkLabelBytecodeInstructions(code: String, tpe: String)(checkOutput: List[Instruction] => Unit): Unit = {
    val source =
      s"""import scala.util.boundary, boundary.break
         |class Test:
         |  def test: $tpe = {
         |    ${code.linesIterator.toList.mkString("", "\n    ", "")}
         |  }
         |  def nonLocalBreak[T](value: T)(using boundary.Label[T]): Nothing = break(value)
         |  def nonLocalBreak()(using boundary.Label[Unit]): Nothing = break(())
      """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Test.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val method  = getMethod(clsNode, "test")

      checkOutput(instructionsFromMethod(method))
    }
  }

}
