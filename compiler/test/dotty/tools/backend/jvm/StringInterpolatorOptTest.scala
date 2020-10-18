package dotty.tools
package backend.jvm

import org.junit.Assert._
import org.junit.Test

class StringInterpolatorOptTest extends DottyBytecodeTest {
  import ASMConverters._

  @Test def testRawInterpolator = {
    val source =
      """
        |class Foo {
        |  val one = 1
        |  val two = "two"
        |  val three = 3.0
        |
        |  def meth1: String = raw"$one plus $two$three\n"
        |  def meth2: String = "" + one + " plus " + two + three + "\\n"
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Foo.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val meth1   = getMethod(clsNode, "meth1")
      val meth2   = getMethod(clsNode, "meth2")

      val instructions1 = instructionsFromMethod(meth1)
      val instructions2 = instructionsFromMethod(meth2)

      assert(instructions1 == instructions2,
        "the `` string interpolator incorrectly converts to string concatenation\n" +
          diffInstructions(instructions1, instructions2))
    }
  }

  @Test def testSInterpolator = {
    val source =
      """
         |class Foo {
         |  val one = 1
         |  val two = "two"
         |  val three = 3.0
         |
         |  def meth1: String = s"$one plus $two$three\n"
         |  def meth2: String = "" + one + " plus " + two + three + "\n"
         |}
      """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Foo.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val meth1   = getMethod(clsNode, "meth1")
      val meth2   = getMethod(clsNode, "meth2")

      val instructions1 = instructionsFromMethod(meth1)
      val instructions2 = instructionsFromMethod(meth2)

      assert(instructions1 == instructions2,
        "the `s` string interpolator incorrectly converts to string concatenation\n" +
          diffInstructions(instructions1, instructions2))
    }
  }
}
