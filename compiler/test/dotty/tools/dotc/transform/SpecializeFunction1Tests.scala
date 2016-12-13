package dotty.tools
package dotc
package transform

import org.junit.Assert._
import org.junit.Test

import dotty.tools.backend.jvm.DottyBytecodeTest

class SpecializeFunction1Tests extends DottyBytecodeTest {

  import dotty.tools.backend.jvm.ASMConverters._
  import dotty.tools.backend.jvm.AsmNode._

  @Test def specializeParentIntToInt = {
    val source = """
                 |class Foo extends Function1[Int, Int] {
                 |  def apply(i: Int) = i
                 |}
                 """.stripMargin

    checkBCode(source) { dir =>
      import scala.collection.JavaConverters._
      val clsIn = dir.lookupName("Foo.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      assert(clsNode.name == "Foo", s"inspecting wrong class: ${clsNode.name}")
      val methods = clsNode.methods.asScala

      val applys = methods
        .collect {
          case m if m.name == "apply$mcII$sp" => m
        }
        .toList

      assert(applys.length == 1, "Wrong number of specialized applys")
    }
  }
}
