package dotty.tools.backend.jvm

import org.junit.Test
import org.junit.Assert._

import scala.tools.asm.Opcodes._

class IincTest extends DottyBytecodeTest {
  import ASMConverters._

  @Test def increment = test(
    """{
      |  var i = x
      |  i += 1
      |  i += 54
      |  i += 127
      |  i -= 1
      |  i -= 54
      |  i -= 128
      |  i
      |}""".stripMargin,
    List(1, 54, 127, -1, -54, -128)
  )

  @Test def wideIncrement = test(
    """{
      |  var i = x
      |  i += 128
      |  i += 8765
      |  i += 32767
      |  i -= 129
      |  i -= 8765
      |  i -= 32768
      |  i
      |}""".stripMargin,
    List(128, 8765, 32767, -129, -8765, -32768)
  )

  @Test def tooBigForIinc = test(
    """{
      |  var i = x
      |  i += 32768
      |  i += 56789
      |  i += 2147483647
      |  i -= 32769
      |  i -= 56789
      |  i -= 2147483647
      |  i
      |}""".stripMargin,
    Nil
  )

  private def test(code: String, expectedIincs: List[Int])= {
    val source =
      s"""class Increment {
         | def test(x: Int): Int = $code
         |}
       """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Increment.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val meth   = getMethod(clsNode, "test")

      val foundIincs = instructionsFromMethod(meth).collect { case iinc: Incr => iinc.incr }

      assertEquals(expectedIincs, foundIincs)
    }
  }

}
