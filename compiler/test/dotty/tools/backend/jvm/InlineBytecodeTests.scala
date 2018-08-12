package dotty.tools.backend.jvm

import org.junit.Assert._
import org.junit.Test

class InlineBytecodeTests extends DottyBytecodeTest {
  import ASMConverters._
  @Test def inlineUnit = {
    val source = """
                 |class Foo {
                 |  rewrite def foo: Int = 1
                 |  @forceInline def bar: Int = 1
                 |
                 |  def meth1: Unit = foo
                 |  def meth2: Unit = bar
                 |  def meth3: Unit = 1
                 |}
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Foo.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)
      val meth1      = getMethod(clsNode, "meth1")
      val meth2      = getMethod(clsNode, "meth2")
      val meth3      = getMethod(clsNode, "meth3")

      val instructions1 = instructionsFromMethod(meth1)
      val instructions2 = instructionsFromMethod(meth2)
      val instructions3 = instructionsFromMethod(meth3)

      assert(instructions1 == instructions3,
        "`foo` was not properly inlined in `meth1`\n" +
        diffInstructions(instructions1, instructions3))

      assert(instructions2 == instructions3,
        "`bar` was not properly inlined in `meth2`\n" +
        diffInstructions(instructions2, instructions3))
    }
  }
}
