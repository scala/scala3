package dotty.tools
package dotc
package transform

import org.junit.Assert._
import org.junit.Test

import dotty.tools.backend.jvm.DottyBytecodeTest

class SpecializeFunctionsTests extends DottyBytecodeTest {

  import dotty.tools.backend.jvm.ASMConverters._
  import dotty.tools.backend.jvm.AsmNode._

  protected def checkForBoxing(ins: List[Instruction], source: String): Unit = ins.foreach {
    case Invoke(op, owner, name, desc, itf) =>
      def error =
        s"""|----------------------------------
            |${ins.mkString("\n")}
            |----------------------------------
            |From code:
            |$source
            |----------------------------------""".stripMargin

      assert(!owner.toLowerCase.contains("box"), s"Boxing instruction discovered in:\n$error")
      assert(!name.toLowerCase.contains("box"), s"Boxing instruction discovered in:\n$error")
    case _ => ()
  }

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
          case m if m.name == "apply" => m
        }
        .map(_.name)
        .toList

      assert(
        // there should be two "apply", one generic and the one overwritten and
        // then the specialized one
        applys.length == 3,
        s"Wrong number of specialized applys, actual length: ${applys.length} $applys"
      )
      assert(
        applys.contains("apply"),
        "Foo did not contain `apply` forwarder method"
      )
      assert(
        applys.contains("apply$mcII$sp"),
        "Foo did not contain specialized apply"
      )
    }
  }

  @Test def checkBoxingIntToInt = {
    val source =
      """|object Test {
         |  class Func1 extends Function1[Int, Int] {
         |    def apply(i: Int) = i + 1
         |  }
         |
         |  (new Func1)(1)
         |}""".stripMargin

    checkBCode(source) { dir =>
      import scala.collection.JavaConverters._
      val clsIn = dir.lookupName("Test$.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      assert(clsNode.name == "Test$", s"inspecting wrong class: ${clsNode.name}")

      clsNode.methods.asScala
        .find(_.name == "<init>")
        .map { m =>
          checkForBoxing(instructionsFromMethod(m), source)
        }
        .getOrElse(assert(false, "Could not find constructor for object `Test`"))
    }
  }

  @Test def specializeFunction2 = {
    val source =
      """|class Func2 extends Function2[Int, Int, Int] {
         |    def apply(i: Int, j: Int): Int = i + j
         |}""".stripMargin

    checkBCode(source) { dir =>
      import scala.collection.JavaConverters._
      val clsIn = dir.lookupName("Func2.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      assert(clsNode.name == "Func2", s"inspecting wrong class: ${clsNode.name}")
      val methods = clsNode.methods.asScala

      val apps = methods
        .collect {
          case m if m.name == "apply$mcIII$sp" => m
          case m if m.name == "apply" => m
        }
        .map(_.name)
        .toList

      assert(
        apps.length == 3,
        s"Wrong number of specialized applys, actual length: ${apps.length} - $apps"
      )
      assert(apps.contains("apply"), "Func3 did not contain `apply` forwarder method")
      assert(apps.contains("apply$mcIII$sp"), "Func3 did not contain specialized apply")
    }
  }
}
