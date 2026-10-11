package dotty.tools
package backend.jvm

import dotty.DottyBytecodeTest
import org.junit.Test
import org.junit.Assert.*


class MultiCatchTest extends DottyBytecodeTest {
  import dotty.AsmConverters.*
  @Test
  def nameless = {
    val source = """
    class A extends Exception
    class B extends Exception
    class X {
      def test() =
        try
          throw new Exception()
        catch
          case _: (A | B) => println("A|B")
          case _ => ()
    }
    """.stripMargin
    checkBCode(source){dir =>
      val clsIn   = lookupClass(dir, "X.class")
      val clsNode = loadClassNode(clsIn)
      val meth   = getMethod(clsNode, "test")
      val instructions = instructionsFromMethod(meth)
      val handlers = convertMethod(meth).handlers
      println("DEBUG: "+instructions)
      println("DEBUG: " + handlers.mkString("\n\n"))
    }
  }


}